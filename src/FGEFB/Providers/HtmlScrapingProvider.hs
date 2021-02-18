{-#LANGUAGE DeriveFunctor #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE ScopedTypeVariables #-}
module FGEFB.Providers.HtmlScrapingProvider
where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import qualified Data.Aeson as JSON
import qualified Data.Aeson.TH as JSON
import Network.HTTP.Simple (httpJSON, httpBS)
import qualified Network.HTTP.Simple as HTTP
import Text.Read (readMaybe)
import System.FilePath (takeBaseName, dropExtension, (</>), (<.>))
import Control.Monad (when, forM)
import qualified Text.HTML.DOM as HTML
import qualified Text.XML.Cursor as XML
import qualified Text.XML.Scraping as XML
import qualified Text.XML.Selector as XML
import qualified Text.XML as XML
import Data.Maybe (catMaybes, listToMaybe, maybeToList)
import Network.HTTP.Base (urlEncode, urlDecode)
import qualified Data.ByteString.Lazy as LBS
import Data.Text.Encoding (decodeUtf8)
import Text.Printf (printf)
import qualified Data.Aeson as JSON
import Data.Aeson ( (.:), (.:?), (.!=) )

import FGEFB.Provider
import FGEFB.LoadPDF
import FGEFB.URL (renderURL, parseURL, URL (..))

data Extraction =
  Extraction
    { extractionSelector :: Maybe Text
    , extractionTarget :: ExtractionTarget
    }

data ExtractionTarget
  = ExtractText
  | ExtractAttribute XML.Name

instance JSON.FromJSON Extraction where
  parseJSON j = case j of
    JSON.String t ->
      case Text.take 1 t of
        "" -> return (Extraction Nothing ExtractText)
        "@" -> return (Extraction Nothing (ExtractAttribute . uqName $ Text.drop 1 t))
        _ -> return (Extraction (Just t) ExtractText)
    _ -> JSON.withObject "Extraction" goObj j
    where
      goObj obj = do
        mselector <- obj .:? "child"
        mattrib <- fmap uqName <$> (obj .:? "attrib")
        let target = case mattrib of
                        Nothing -> ExtractText
                        Just a -> ExtractAttribute a
        return $ Extraction mselector target

extract :: Extraction -> XML.Cursor -> Maybe Text
extract e root = do
  elem <- (maybe Just (XML.query1 . Text.unpack) (extractionSelector e)) root
  case extractionTarget e of
    ExtractText ->
      return (LText.toStrict $ XML.innerText elem)
    ExtractAttribute n ->
      listToMaybe $ XML.attribute n elem

data LinkSpec =
  LinkSpec
    { elemSelector :: Text
    , linkHrefExtraction :: Extraction
    , linkLabelExtraction :: Extraction
    }

instance JSON.FromJSON LinkSpec where
  parseJSON j = JSON.withObject "LinkSpec" goObj j
    where
      goObj obj = do
        LinkSpec
          <$> obj .: "select"
          <*> obj .:? "href" .!= (Extraction Nothing (ExtractAttribute "href"))
          <*> obj .:? "label" .!= (Extraction Nothing ExtractText)

extractLinks :: LinkSpec -> XML.Cursor -> [(Text, URL)]
extractLinks spec root = do
  sel <- Text.unpack . Text.strip <$> Text.splitOn "," (elemSelector spec)
  elem <- XML.query sel root
  label <- maybeToList $ extract (linkLabelExtraction spec) elem
  href <- maybeToList $ extract (linkHrefExtraction spec) elem
  url <- either (const []) return $ parseURL href
  return (label, url)

htmlScrapingProvider :: Maybe Text
                     -> Text -- ^ root URL
                     -> Text -- ^ landing path
                     -> LinkSpec -- ^ folders
                     -> LinkSpec -- ^ documents
                     -> Provider
htmlScrapingProvider mlabel rootUrlStr landingPath folderSpec documentSpec =
  Provider
    { label = mlabel
    , getPdfPage = \filenameEnc page -> do
        let filename = urlDecode . dropExtension $ filenameEnc
            localURL = either error id . parseURL . Text.pack $ filename
        loadPdfPageHttp (renderURL $ rootURL <> localURL) page
    , listFiles = \providerID pathEnc -> do
        let path = urlDecode pathEnc
        putStrLn path
        let actualPath = if null path then landingPath else Text.pack path
            actualURL = either error id . parseURL $ actualPath
        (parentPath, document) <- fetchListing (renderURL $ rootURL <> actualURL)
        let currentURL = either error id $ parseURL parentPath
        let root = XML.fromDocument document
            folderInfos =
              map
                (makeLink True currentURL providerID)
                (extractLinks folderSpec root)
            documentInfos =
              map
                (makeLink False currentURL providerID)
                (extractLinks documentSpec root)
        return $ folderInfos ++ documentInfos
    }
    where
      rootURL :: URL
      rootURL = either error id $ parseURL rootUrlStr

      makeLink :: Bool -> URL -> Text -> (Text, URL) -> FileInfo
      makeLink isDir currentURL providerID (label, linkUrl) =
        FileInfo
           { fileName = Text.unwords . Text.words $ label
           , filePath = if isDir then path else path <.> ".pdf"
           , fileType = if isDir then Directory else PDFFile
           }
        where
          -- relative URLs resolved against current URL, and then made
          -- hostname-relative
          url :: URL
          url = (currentURL <> linkUrl)
                  { urlHostName = Nothing, urlProtocol = Nothing }

          path :: FilePath
          path = Text.unpack providerID </> (urlEncode . Text.unpack $ renderURL url)

fetchListing :: Text -> IO (Text, XML.Document)
fetchListing url = do
  printf "HTTP %s\n" url
  rq <- HTTP.parseRequest (Text.unpack url)
  rp <- HTTP.httpLBS rq
  if HTTP.getResponseStatusCode rp `elem` redirectCodes then do
    let mlocation = lookup "Location" $ HTTP.getResponseHeaders rp
    case mlocation of
      Nothing ->
        error "Missing location header"
      Just location -> do
        let url' = decodeUtf8 location
        if url' == url then
          error "Infinite redirection"
        else
          fetchListing url'
  else do
    let document = HTML.parseLBS . HTTP.getResponseBody $ rp
        metaRefresh =
          map (combineURLs url) .
          catMaybes .
          map (parseMetaRefresh . mconcat . XML.attribute "content") .
          XML.query "meta[http-equiv=Refresh]" .
          XML.fromDocument $
          document
    case metaRefresh of
      url':_ ->
        if url' == url then
          error "Infinite redirection"
        else
          fetchListing url'
      [] ->
        return $ (url, document)
  where
    redirectCodes = [301, 302, 303, 307, 308]
    parseMetaRefresh :: Text -> Maybe Text
    parseMetaRefresh content =
      let parts = map Text.strip $ Text.splitOn ";" content
      in case parts of
        (_ : url : _) ->
          if "url=" `Text.isPrefixOf` url then
            Just $ Text.drop 4 url
          else
            Nothing
        _ ->
          Nothing

for = flip map

combineURLs :: Text -> Text -> Text
combineURLs current new =
  either error id $ do
    currentURL <- parseURL current
    newURL <- parseURL new
    return $ renderURL (currentURL <> newURL)

uqName :: Text -> XML.Name
uqName t = XML.Name t Nothing Nothing
