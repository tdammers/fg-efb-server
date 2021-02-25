{-#LANGUAGE DeriveFunctor #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE LambdaCase #-}
module FGEFB.Providers.HtmlScrapingProvider
where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import qualified Data.Aeson as JSON
import qualified Data.Aeson.TH as JSON
import Network.HTTP.Simple (httpJSON, httpBS)
import qualified Network.HTTP.Simple as HTTP
import qualified Network.HTTP.Conduit as HTTP
import qualified Network.HTTP.Types as HTTP
import Text.Read (readMaybe)
import System.FilePath (takeBaseName, dropExtension, (</>), (<.>))
import Control.Monad (when, forM)
import qualified Text.HTML.DOM as HTML
import qualified Text.XML.Cursor as XML
import qualified Text.XML.Selectors as XML
import qualified Text.XML.Selectors.Parsers.JQ as XML
import qualified Text.XML as XML
import Data.Maybe (catMaybes, listToMaybe, maybeToList, fromMaybe)
import Network.HTTP.Base (urlEncode, urlDecode)
import qualified Data.ByteString.Lazy as LBS
import Data.Text.Encoding (decodeUtf8)
import Text.Printf (printf)
import qualified Data.Aeson as JSON
import Data.Aeson ( (.:), (.:?), (.!=) )
import Debug.Trace (trace, traceShow, traceM, traceShowM)
import qualified Data.Vector as Vector
import Data.List (foldl', sortOn)
import Text.Casing as Casing
import Data.Time
import qualified Data.Map as Map

import FGEFB.Provider
import FGEFB.LoadPDF
import FGEFB.URL (renderURL, parseURL, URL (..), normalizeURL)
import FGEFB.Util
import FGEFB.Regex

data ExtractionOf t =
  Extraction
    { extractionSelector :: Maybe t
    , extractionTarget :: ExtractionTarget
    }
    deriving (Show, Functor)

type Extraction = ExtractionOf Text

data ExtractionTarget
  = ExtractText
  | ExtractAttribute XML.Name
  deriving (Show)

instance JSON.FromJSON Extraction where
  parseJSON j = case j of
    JSON.Null ->
      return (Extraction Nothing ExtractText)
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

extract :: Extraction -> XML.Cursor -> [Text]
extract e root = do
  elem <- (maybe (:[]) jqQuery (extractionSelector e)) root
  case extractionTarget e of
    ExtractText ->
      return . textContent . XML.node $ elem
    ExtractAttribute n ->
      XML.attribute n elem

data LabelFormat
  = LabelIdentity
  | LabelBasename
  | LabelReplace Text Text
  | LabelRegexReplace Text Text
  | LabelSplitHumps
  | LabelSplitOn Text
  | LabelFormatList [LabelFormat]
  deriving (Show)

instance JSON.FromJSON LabelFormat where
  parseJSON (JSON.String t) =
    case t of
      "basename" -> return LabelBasename
      "split-humps" -> return LabelSplitHumps
      _ -> fail "Invalid label format"
  parseJSON (JSON.Array xs) =
    LabelFormatList <$> mapM JSON.parseJSON (Vector.toList xs)
  parseJSON j =
    flip (JSON.withObject "LabelFormat") j $ \obj -> do
      mreplace <- fmap (\(needle, haystack) -> LabelReplace needle haystack) <$>
                    obj .:? "replace"
      mregex <- fmap (\(needle, haystack) -> LabelRegexReplace needle haystack) <$>
                    obj .:? "regex"
      msplit <- fmap LabelSplitOn <$> obj .:? "split"
      return . LabelFormatList . catMaybes $ [mregex, mreplace, msplit]

formatLabel :: LabelFormat -> Text -> Text
formatLabel LabelIdentity = id
formatLabel LabelBasename = Text.pack . takeBaseName . Text.unpack
formatLabel LabelSplitHumps = Text.pack . Casing.toWords . Casing.fromHumps . Text.unpack
formatLabel (LabelReplace needle replacement) = Text.replace needle replacement
formatLabel (LabelSplitOn sep) = Text.unwords . Text.splitOn sep
formatLabel (LabelFormatList items) = \src -> foldl' (flip formatLabel) src items
formatLabel (LabelRegexReplace re replacement) =
  reReplace re replacement

data LinkSpec =
  LinkSpec
    { elemSelector :: Text
    , linkHrefExtraction :: Extraction
    , linkHrefFormat :: LabelFormat
    , linkLabelExtraction :: Extraction
    , linkLabelFormat :: LabelFormat
    , linkAutoFollow :: Bool
    , linkContext :: Maybe ContextPattern
    , linkFilter :: Maybe Text
    , linkSort :: LinkSort
    }
    deriving (Show)

data LinkSort
  = SortLinkNone
  | SortLinkByLabel
  deriving (Show)

instance JSON.FromJSON LinkSort where
  parseJSON (JSON.String "none") = return SortLinkNone
  parseJSON (JSON.String "label") = return SortLinkByLabel
  parseJSON (JSON.Bool True) = return SortLinkByLabel
  parseJSON (JSON.Bool False) = return SortLinkNone
  parseJSON _ = fail "Invalid sorting"

type ContextPattern = Text

matchLinkContext :: URL -> LinkSpec -> Bool
matchLinkContext url spec =
  case linkContext spec of
    Nothing -> True
    Just pattern -> matchPatternContext url pattern

matchPatternContext :: URL -> Text -> Bool
matchPatternContext url' pattern' =
  case Text.take 1 pattern of
    "^" -> Text.drop 1 pattern `Text.isSuffixOf` renderURL url
    "!" -> not . matchPatternContext url $ Text.drop 1 pattern
    _ -> renderURL url == pattern
    where
      url = makeHostRelative url'
      pattern = Text.replace
                  "{anchor}"
                  (fromMaybe "" $ urlAnchor url)
                  pattern'

instance JSON.FromJSON LinkSpec where
  parseJSON j = JSON.withObject "LinkSpec" goObj j
    where
      goObj obj = do
        LinkSpec
          <$> obj .: "select"
          <*> obj .:? "href" .!= (Extraction Nothing (ExtractAttribute "href"))
          <*> obj .:? "href-format" .!= LabelIdentity
          <*> obj .:? "label" .!= (Extraction Nothing ExtractText)
          <*> obj .:? "format" .!= LabelIdentity
          <*> obj .:? "auto-follow" .!= False
          <*> obj .:? "context"
          <*> obj .:? "filter"
          <*> obj .:? "sort" .!= SortLinkNone

extractLinks :: URL -> LinkSpec -> XML.Cursor -> [(Text, URL, Bool)]
extractLinks currentURL spec root = applySorting $ do
  sel <- Text.strip <$> Text.splitOn "," (replaceVars $ elemSelector spec)
  elem <- jqQuery sel root
  let labelRaw = Text.unwords $ extract (replaceVars <$> linkLabelExtraction spec) elem
  let label = formatLabel (linkLabelFormat spec) labelRaw

  let keep = case linkFilter spec of
                Nothing -> True
                Just re -> reTest re label
  if keep then do
    hrefRaw <- extract (replaceVars <$> linkHrefExtraction spec) elem
    let href = formatLabel (linkHrefFormat spec) hrefRaw
    url <- either (const []) return $ parseURL href
    return (label, url, linkAutoFollow spec)
  else do
    []
  where
    replaceVars =
      Text.replace "{anchor}" $ fromMaybe "" (urlAnchor currentURL)
    applySorting =
      case linkSort spec of
        SortLinkNone -> id
        SortLinkByLabel -> sortOn (\(label, _, _) -> label)
        

htmlScrapingProvider :: ProviderContext
                     -> Maybe Text
                     -> Text -- ^ root URL
                     -> Text -- ^ landing path
                     -> [LinkSpec] -- ^ folders
                     -> [LinkSpec] -- ^ documents
                     -> Provider
htmlScrapingProvider context mlabel rootUrlTemplate landingPathTemplate folderSpecs documentSpecs =
  Provider
    { label = mlabel
    , getPdfPage = \filenameEnc page -> do
        let filename = urlDecode . Text.unpack $ filenameEnc
            localURL = either error id . parseURL . Text.pack $ filename
        loadPdfPageHttp (renderURL $ rootURL <> localURL) page
    , listFiles = \pathEnc -> do
        let go :: Text -> IO [FileInfo]
            go path = do
              let actualPath = if Text.null path then landingPath else path
                  actualURL = normalizeURL . either error id . parseURL $ actualPath
              (parentPath, document) <- fetchListing (renderURL $ rootURL <> actualURL)
              let currentURL = either error id $ parseURL parentPath
              let root = XML.fromDocument document
                  folderLinks =
                    concatMap
                      (\folderSpec -> extractLinks currentURL folderSpec root)
                      (filter (matchLinkContext currentURL) folderSpecs)
                  autofollowLinks = [ url | (_, url, True) <- folderLinks ]
              case autofollowLinks of
                [] -> do
                  let folderInfos = map (makeLink True currentURL) folderLinks
                      documentInfos =
                        concatMap (\documentSpec ->
                          map
                            (makeLink False currentURL)
                            (extractLinks currentURL documentSpec root))
                          (filter (matchLinkContext currentURL) documentSpecs)
                  return $ folderInfos ++ documentInfos
                linkUrl:_ -> do
                  let url = (currentURL <> linkUrl)
                          { urlHostName = Nothing, urlProtocol = Nothing }

                  printf "Auto-following %s -> %s\n" path (renderURL url)
                  go $ renderURL url
        let path = Text.pack . urlDecode . Text.unpack $ pathEnc
        go path
        
    }
    where
      vars = unpackVars (contextDefs context)

      rootUrlStr :: Text
      rootUrlStr = interpolateVars vars rootUrlTemplate

      landingPath :: Text
      landingPath = interpolateVars vars landingPathTemplate

      rootURL :: URL
      rootURL = either error id $ parseURL rootUrlStr

      makeLink :: Bool -> URL -> (Text, URL, Bool) -> FileInfo
      makeLink isDir currentURL (label, linkUrl, _) =
        FileInfo
           { fileName = Text.unwords . Text.words $ label
           , filePath = path
           , fileType = if isDir then Directory else PDFFile
           }
        where
          -- relative URLs resolved against current URL, and then made
          -- hostname-relative
          url :: URL
          url = makeHostRelative (currentURL <> linkUrl)

          path :: Text
          path = withString urlEncode $ renderURL url

makeHostRelative :: URL -> URL
makeHostRelative url = url { urlHostName = Nothing, urlProtocol = Nothing }

fetchListing :: Text -> IO (Text, XML.Document)
fetchListing url = go url 12
  where
    go :: Text -> Int -> IO (Text, XML.Document)
    go url 0 = error "Maximum redirect count exceeded"
    go url n = do
      printf "HTTP GET %s\n" url
      rq <- HTTP.parseRequest (Text.unpack url)
      rp <- HTTP.httpLBS rq { HTTP.redirectCount = 0 }
      printf "HTTP %i %s\n"
        (HTTP.getResponseStatusCode rp)
        (decodeUtf8 . HTTP.statusMessage $ HTTP.getResponseStatus rp)
      if HTTP.getResponseStatusCode rp `elem` redirectCodes then do
        let mlocation = lookup "Location" $ HTTP.getResponseHeaders rp
        case mlocation of
          Nothing ->
            error "Missing location header"
          Just location -> do
            printf "Redirecting due to Location header: %s\n" (decodeUtf8 location)
            let url' = renderURL $
                        either error id (parseURL url) <>
                        either error id (parseURL (decodeUtf8 location))
            if url' == url then
              error "Infinite redirection"
            else do
              go url' (n - 1)
      else do
        let document = HTML.parseLBS . HTTP.getResponseBody $ rp
            metaRefresh =
              map (combineURLs url) .
              catMaybes .
              map (parseMetaRefresh . mconcat . XML.attribute "content") .
              jqQuery ("meta[http-equiv=Refresh]" :: Text) .
              XML.fromDocument $
              document
        case metaRefresh of
          url':_ -> do
            printf "Redirecting due to meta refresh: %s\n" url'
            if url' == url then
              error "Infinite redirection"
            else
              go url' (n - 1)
          [] -> do
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

interpolateVars :: [(Text, Text)] -> Text -> Text
interpolateVars vars template =
  foldl' (flip interpolateVar) template vars

interpolateVar :: (Text, Text) -> Text -> Text
interpolateVar (k, v) =
  Text.replace ("{" <> k <> "}") v

jqQuery :: Text -> XML.Cursor -> [XML.Cursor]
jqQuery q = XML.match $ XML.jqText' q

jqQuery1 :: Text -> XML.Cursor -> Maybe XML.Cursor
jqQuery1 q = listToMaybe . jqQuery q

textContent :: XML.Node -> Text
textContent (XML.NodeContent t) = t
textContent (XML.NodeElement (XML.Element _ _ children)) = mconcat . map textContent $ children
textContent _ = ""
