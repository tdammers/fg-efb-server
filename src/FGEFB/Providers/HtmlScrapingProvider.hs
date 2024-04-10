{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module FGEFB.Providers.HtmlScrapingProvider
where

import Data.Aeson ( (.:), (.:?), (.!=) )
import qualified Data.Aeson as JSON
import Data.List (foldl', sortOn)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Vector as Vector
import Network.HTTP.Base (urlEncode, urlDecode)
import System.FilePath (takeBaseName)
import Text.Casing as Casing
import qualified Text.HTML.DOM as HTML
import Text.Printf (printf)
import qualified Text.XML as XML
import qualified Text.XML.Cursor as XML

import FGEFB.Provider
import FGEFB.Regex
import FGEFB.URL (renderURLText, parseURLText, URL (..), normalizeURL)
import FGEFB.Util
import FGEFB.XmlUtil
import FGEFB.HTTP (httpGET, downloadHttp)

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
        let target = maybe ExtractText ExtractAttribute mattrib
        return $ Extraction mselector target

extract :: Extraction -> XML.Cursor -> [Text]
extract e root = do
  xmlElem <- maybe (:[]) jqQuery (extractionSelector e) root
  case extractionTarget e of
    ExtractText ->
      return . textContent . XML.node $ xmlElem
    ExtractAttribute n ->
      XML.attribute n xmlElem

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
    "^" -> Text.drop 1 pattern `Text.isSuffixOf` renderURLText url
    "!" -> not . matchPatternContext url $ Text.drop 1 pattern
    _ -> renderURLText url == pattern
    where
      url = makeHostRelative url'
      pattern = Text.replace
                  "{anchor}"
                  (decodeUtf8 . fromMaybe "" $ urlAnchor url)
                  pattern'

instance JSON.FromJSON LinkSpec where
  parseJSON = JSON.withObject "LinkSpec" goObj
    where
      goObj obj = do
        LinkSpec
          <$> obj .: "select"
          <*> obj .:? "href" .!= Extraction Nothing (ExtractAttribute "href")
          <*> obj .:? "href-format" .!= LabelIdentity
          <*> obj .:? "label" .!= Extraction Nothing ExtractText
          <*> obj .:? "format" .!= LabelIdentity
          <*> obj .:? "auto-follow" .!= False
          <*> obj .:? "context"
          <*> obj .:? "filter"
          <*> obj .:? "sort" .!= SortLinkNone

extractLinks :: URL -> LinkSpec -> XML.Cursor -> [(Text, URL, Bool)]
extractLinks currentURL spec root = applySorting $ do
  sel <- Text.strip <$> Text.splitOn "," (replaceVars $ elemSelector spec)
  xmlElem <- jqQuery sel root
  let labelRaw = Text.unwords $ extract (replaceVars <$> linkLabelExtraction spec) xmlElem
  let labelFormatted = formatLabel (linkLabelFormat spec) labelRaw

  let keep = case linkFilter spec of
                Nothing -> True
                Just re -> reTest re labelFormatted
  if keep then do
    hrefRaw <- extract (replaceVars <$> linkHrefExtraction spec) xmlElem
    let href = formatLabel (linkHrefFormat spec) hrefRaw
    url <- either (const []) return $ parseURLText href
    return (labelFormatted, url, linkAutoFollow spec)
  else do
    []
  where
    replaceVars =
      Text.replace "{anchor}" . decodeUtf8 $ fromMaybe "" (urlAnchor currentURL)
    applySorting =
      case linkSort spec of
        SortLinkNone -> id
        SortLinkByLabel -> sortOn (\(l, _, _) -> l)
        

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
    , getPdf = \_ filenameEnc -> do
        let filename = urlDecode . Text.unpack $ filenameEnc
        let localURL = either error id . parseURLText . Text.pack $ filename
        Just <$> downloadHttp (renderURLText $ rootURL <> localURL) ".pdf"
    , listFiles = \_ pathEnc page -> do
        let go :: Text -> IO [FileInfo]
            go path = do
              let actualPath = if Text.null path then landingPath else path
              let actualURL = normalizeURL . either error id . parseURLText $ actualPath
              (parentPath, document) <- fetchListing (renderURLText $ rootURL <> actualURL)
              let currentURL = either error id $ parseURLText parentPath
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

                  printf "Auto-following %s -> %s\n" path (renderURLText url)
                  go $ renderURLText url
        let path = Text.pack . urlDecode . Text.unpack $ pathEnc
        paginate page <$> go path
        
    }
    where
      vars = unpackVars (contextDefs context)

      rootUrlStr :: Text
      rootUrlStr = interpolateVars vars rootUrlTemplate

      landingPath :: Text
      landingPath = interpolateVars vars landingPathTemplate

      rootURL :: URL
      rootURL = either error id $ parseURLText rootUrlStr

      makeLink :: Bool -> URL -> (Text, URL, Bool) -> FileInfo
      makeLink isDir currentURL (linkLabel, linkUrl, _) =
        FileInfo
           { fileName = Text.unwords . Text.words $ linkLabel
           , filePath = path
           , fileType = if isDir then Directory else PDFFile
           }
        where
          -- relative URLs resolved against current URL, and then made
          -- hostname-relative
          url :: URL
          url = makeHostRelative (currentURL <> linkUrl)

          path :: Text
          path = withString urlEncode $ renderURLText url

makeHostRelative :: URL -> URL
makeHostRelative url = url { urlHostName = Nothing, urlProtocol = Nothing }

fetchListing :: Text -> IO (Text, XML.Document)
fetchListing url = do
  (url', rawBody) <- httpGET url
  let document = HTML.parseLBS rawBody
  return (url', document)

for :: [a] -> (a -> b) -> [b]
for = flip map

combineURLs :: Text -> Text -> Text
combineURLs current new =
  either error id $ do
    currentURL <- parseURLText current
    newURL <- parseURLText new
    return $ renderURLText (currentURL <> newURL)
