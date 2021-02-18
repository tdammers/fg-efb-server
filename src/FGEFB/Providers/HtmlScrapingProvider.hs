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
import qualified Text.XML.Cursor as HTML
import qualified Text.XML.Scraping as HTML
import qualified Text.XML.Selector as HTML
import qualified Text.XML as XML
import Data.Maybe (catMaybes)
import Network.HTTP.Base (urlEncode, urlDecode)
import qualified Data.ByteString.Lazy as LBS
import Data.Text.Encoding (decodeUtf8)
import Text.Printf (printf)

import FGEFB.Provider
import FGEFB.LoadPDF
import FGEFB.URL (renderURL, parseURL, URL (..))

htmlScrapingProvider :: Maybe Text
                     -> Text -- ^ root URL
                     -> Text -- ^ landing path
                     -> Text -- ^ selector for folder links
                     -> Maybe String -- ^ selector for extracting folder URL from folder link
                     -> Maybe Text -- ^ attribute name for extracting folder URL from folder link
                     -> Maybe String -- ^ selector for extracting folder label from folder link
                     -> Maybe Text -- ^ attribute name for extracting folder label from folder link
                     -> Text -- ^ selector for document links
                     -> Maybe String -- ^ selector for extracting document URL from document link
                     -> Maybe Text -- ^ attribute name for extracting document URL from document link
                     -> Maybe String -- ^ selector for extracting document label from document link
                     -> Maybe Text -- ^ attribute name for extracting document label from document link
                     -> Provider
htmlScrapingProvider
  mlabel
  rootUrlStr landingPath
  folderSel folderUrlSel folderUrlAttrib folderLabelSel folderLabelAttrib
  documentSel documentUrlSel documentUrlAttrib documentLabelSel documentLabelAttrib =
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
        let root = HTML.fromDocument document
            folderInfos = captureLinks
                            currentURL
                            providerID
                            Directory
                            folderSel
                            folderUrlSel
                            folderUrlAttrib
                            folderLabelSel
                            folderLabelAttrib
                            root
            documentInfos = captureLinks
                              currentURL
                              providerID
                              PDFFile
                              documentSel
                              documentUrlSel
                              documentUrlAttrib
                              documentLabelSel
                              documentLabelAttrib
                              root
        return $ folderInfos ++ documentInfos
    }
    where
      lookupAttrib :: Maybe Text -> HTML.Cursor -> Text
      lookupAttrib Nothing e = LText.toStrict $ HTML.innerText e
      lookupAttrib (Just attrib) e = mconcat $ HTML.attribute (XML.Name attrib Nothing Nothing) e

      rootURL :: URL
      rootURL = either error id $ parseURL rootUrlStr

      captureLinks :: URL
                   -> Text
                   -> FileType
                   -> Text
                   -> Maybe String
                   -> Maybe Text
                   -> Maybe String
                   -> Maybe Text
                   -> HTML.Cursor
                   -> [FileInfo]
      captureLinks currentURL providerID fty elemSels urlSel urlAttrib labelSel labelAttrib root =
        let elems = concatMap (\elemSel -> HTML.query elemSel root) (map Text.unpack . Text.splitOn "," $ elemSels)
            links = catMaybes $ for elems (\e -> do
                      label <- lookupAttrib labelAttrib <$> maybe Just HTML.query1 labelSel e
                      path <- lookupAttrib urlAttrib <$> maybe Just HTML.query1 urlSel e
                      url <- either (const Nothing) Just $ parseURL path
                      let fullURL = currentURL <> url
                          absURL = fullURL
                                    { urlHostName = Nothing
                                    , urlProtocol = Nothing
                                    }
                      return (label, renderURL absURL))
            extend = if fty == Directory then id else (<.> "pdf")
        in [ FileInfo
               { fileName = Text.unwords . Text.words $ label
               , filePath =
                  extend $ Text.unpack providerID </> (urlEncode . Text.unpack $ path)
               , fileType = fty
               }
           | (label, path) <- links
           ]

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
          map (parseMetaRefresh . mconcat . HTML.attribute "content") .
          HTML.query "meta[http-equiv=Refresh]" .
          HTML.fromDocument $
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
