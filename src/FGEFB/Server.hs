{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module FGEFB.Server
where

import Control.Exception
import Control.Monad (forM_)
import qualified Data.Aeson as JSON
import Data.Bool (bool)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Cache (Cache, newCache)
import qualified Data.Cache as Cache
import Data.Default (def)
import Data.FileEmbed
import Data.Hashable (Hashable)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, catMaybes, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.IO as LText
import Data.Time (UTCTime (..), getCurrentTime)
import qualified Data.Yaml as YAML
import qualified Network.HTTP.Types as HTTP
import Network.Wai as Wai
import System.Directory
          ( doesFileExist
          , XdgDirectory (..)
          , getXdgDirectory
          , getAppUserDataDirectory
          )
import System.Environment (setEnv, lookupEnv)
import System.FilePath ( (</>) )
import System.IO (hPutStrLn, stderr)
import System.IO.Temp (withSystemTempDirectory)
import qualified Text.XML as XML
import Web.Scotty (ScottyM, scotty)
import qualified Web.Scotty as Scotty

import FGEFB.Airac (Airac, findCurrentAiracOn, airacDetails)
import qualified FGEFB.Airac as Airac
import FGEFB.LoadPDF
import FGEFB.Provider
import FGEFB.Providers
import FGEFB.Providers.GroupProvider
import FGEFB.Providers.LocalFileProvider
import FGEFB.XmlUtil

captureListing :: Wai.Request -> Maybe [Scotty.Param]
captureListing rq =
  case filter (not . Text.null) (Wai.pathInfo rq) of
    "charts":"api":pathItems -> Just [("path", LText.fromStrict $ Text.intercalate "/" pathItems)]
    _ -> Nothing

captureRenderedPage :: Wai.Request -> Maybe [Scotty.Param]
captureRenderedPage rq =
  case filter (not . Text.null) (Wai.pathInfo rq) of
    "charts":"files":pathItems ->
      Just [("path", LText.fromStrict $ Text.intercalate "/" pathItems)]
    _ ->
      Nothing

captureFileInfo :: Wai.Request -> Maybe [Scotty.Param]
captureFileInfo rq =
  case filter (not . Text.null) (Wai.pathInfo rq) of
    "charts":"meta":pathItems ->
      Just [("path", LText.fromStrict $ Text.intercalate "/" pathItems)]
    _ ->
      Nothing

errorPNG :: Scotty.ActionM ()
errorPNG = do
  Scotty.setHeader "Content-Type" "image/png"
  Scotty.raw $ LBS.fromStrict $(embedFile "./static/error.png")

notfoundPNG :: Scotty.ActionM ()
notfoundPNG = do
  Scotty.setHeader "Content-Type" "image/png"
  Scotty.raw $ LBS.fromStrict $(embedFile "./static/notfound.png")

styleXSL :: Scotty.ActionM ()
styleXSL = do
  Scotty.setHeader "Content-Type" "text/xsl"
  Scotty.raw $ LBS.fromStrict $(embedFile "./static/style.xsl")

styleCSS :: Scotty.ActionM ()
styleCSS = do
  Scotty.setHeader "Content-Type" "text/css"
  Scotty.raw $ LBS.fromStrict $(embedFile "./static/style.css")

icons :: Map FilePath ByteString
icons = Map.fromList $(embedDir "./static/icons")

iconPNG :: FilePath -> Scotty.ActionM ()
iconPNG iconName = do
  Scotty.setHeader "Content-Type" "image/png"
  Scotty.raw =<< maybe (Scotty.liftAndCatchIO (putStrLn $ "Icon not found: " ++ iconName) >> Scotty.next) (return . LBS.fromStrict) (Map.lookup iconName icons)

app :: Cache Text [FileInfo] -> Provider -> ScottyM ()
app listingCache provider = do
  Scotty.defaultHandler $ \err -> do
    Scotty.liftAndCatchIO $ LText.putStrLn err
    Scotty.setHeader "Content-type" "text/xml"
    Scotty.raw . XML.renderLBS def . xmlFragmentToDocument $
      XML.Element
        "error"
        Map.empty
        [ XML.NodeContent "Something went wrong." ]
        -- [ XML.NodeContent (LText.toStrict err) ]

  Scotty.get "/" $ do
    Scotty.headers >>= Scotty.liftAndCatchIO . print
    Scotty.redirect "/charts/api"

  Scotty.get "/static/style.css" styleCSS
  Scotty.get "/static/style.xsl" styleXSL
  Scotty.get "/static/error.png" errorPNG
  Scotty.get "/static/notfound.png" notfoundPNG
  Scotty.get "/static/icons/:icon" $ do
    iconName <- Scotty.param "icon"
    iconPNG iconName
      

  -- directory listings
  Scotty.get (Scotty.function captureListing) $ do
    dirname <- Scotty.param "path"
    files <- Scotty.liftAndCatchIO $
      cached listingCache dirname $ listFiles provider dirname
    Scotty.setHeader "Content-type" "text/xml"
    Scotty.raw . XML.renderLBS def . xmlFragmentToDocument $
      xmlFileList files

  -- Page metadata
  Scotty.get (Scotty.function captureFileInfo) $ do
    filename <- Scotty.param "path"
    pdfFilenameMaybe <- Scotty.liftAndCatchIO $ getPdf provider filename
    pdfFilename <- maybe Scotty.next return $ pdfFilenameMaybe
    pdfInfo <- Scotty.liftAndCatchIO $ getPdfInfo pdfFilename
    Scotty.setHeader "Content-type" "text/xml"
    Scotty.raw . XML.renderLBS def . xmlFragmentToDocument $
      xmlFileInfo filename pdfInfo

  -- Pages rendered to JPG
  Scotty.get (Scotty.function captureRenderedPage) $ do
    filename <- Scotty.param "path"
    page <- Scotty.param "p" `Scotty.rescue` const (return 0)
    ty <- Scotty.param "t" `Scotty.rescue` const (return "jpg")

    pdfFilenameMaybe <- Scotty.liftAndCatchIO $ getPdf provider filename
    pdfFilename <- maybe Scotty.next return $ pdfFilenameMaybe

    case ty :: Text of
      "jpg" -> do
        mbody <- Scotty.liftAndCatchIO $ loadPdfPage pdfFilename page
        case mbody of
          Nothing -> Scotty.next
          Just body -> do
            Scotty.setHeader "Content-type" "image/jpeg"
            Scotty.raw body
      "pdf" -> do
        Scotty.setHeader "Content-type" "application/pdf"
        Scotty.file pdfFilename
      _ -> Scotty.next

  Scotty.get (Scotty.function captureRenderedPage) $ do
    Scotty.status HTTP.notFound404
    notfoundPNG

  where
    cached :: (Eq k, Hashable k) => Cache k v -> k -> IO v -> IO v
    cached cache key action = do
      mfound <- Cache.lookup cache key
      case mfound of
        Just found ->
          return found
        Nothing -> do
          value <- action
          Cache.insert cache key value
          return value

xmlFileList :: [FileInfo] -> XML.Element
xmlFileList files =
  XML.Element "listing" [] (map xmlFileEntry files)

xmlFileEntry :: FileInfo -> XML.Node
xmlFileEntry info =
  case fileType info of
    Directory ->
      XML.NodeElement $
        XML.Element "directory"
          []
          [ XML.NodeElement $ XML.Element "name" [] [ XML.NodeContent (fileName info) ]
          , XML.NodeElement $ XML.Element "path" [] [ XML.NodeContent ("/charts/api/" <> filePath info) ]
          , XML.NodeElement $ XML.Element "type" [] [ XML.NodeContent "directory" ]
          ]
    PDFFile ->
      XML.NodeElement $
        XML.Element "file"
          []
          [ XML.NodeElement $ XML.Element "name" [] [ XML.NodeContent (fileName info) ]
          , XML.NodeElement $ XML.Element "path" [] [ XML.NodeContent ("/charts/files/" <> filePath info) ]
          , XML.NodeElement $ XML.Element "meta" [] [ XML.NodeContent ("/charts/meta/" <> filePath info) ]
          , XML.NodeElement $ XML.Element "type" [] [ XML.NodeContent "pdf" ]
          ]

xmlFileInfo :: Text -> [(Text, Text)] -> XML.Element
xmlFileInfo filename infos =
  XML.Element "meta"
    []
    (
      [ XML.NodeElement $ XML.Element "filename" [] [ XML.NodeContent filename ] ]
      <>
      [ XML.NodeElement $ XML.Element "property"
          [ ("name", k)
          , ("value", v)
          ]
          []
      | (k, v) <- infos
      ]
    )

xmlProviderList :: Map Text Provider -> XML.Element
xmlProviderList providers =
  XML.Element "listing" [] (map xmlProviderEntry (Map.toAscList providers))

xmlProviderEntry :: (Text, Provider) -> XML.Node
xmlProviderEntry (key, provider) =
  xmlFileEntry
    FileInfo
      { fileName = fromMaybe key (label provider)
      , filePath = key
      , fileType = Directory
      }

runServerWith :: Map Text Provider -> IO ()
runServerWith providers = do
  lookupEnv "CACHE_DIR" >>= \case
    Nothing -> do
      withSystemTempDirectory "fg-efb-cache" $ \tmpdir -> do
        setEnv "CACHE_DIR" tmpdir
        run
    Just _ -> do
      run
  where
    run = do
      listingCache <- newCache Nothing
      scotty 7675 (app listingCache $ groupProvider (Just "FlightBag") providers)

findConfigFiles :: FilePath -> IO ([FilePath], [FilePath])
findConfigFiles filename = do
  xdg <- getXdgDirectory XdgConfig "fg-efb-server"
  aud <- getAppUserDataDirectory "fg-efb-server"
  let candidates = map (</> filename) [xdg, aud, "."]
  found <- mapM (\f -> bool Nothing (Just f) <$> doesFileExist f) candidates
  return (candidates, catMaybes found)

findFirstConfigFile :: FilePath -> IO ([FilePath], Maybe FilePath)
findFirstConfigFile filename = fmap listToMaybe <$> findConfigFiles filename

loadFirstConfigFile :: JSON.FromJSON a => FilePath -> IO a
loadFirstConfigFile = loadFirstConfigFileOrElse (error "Configuration file not found")

loadFirstConfigFileOrElse :: JSON.FromJSON a => a -> FilePath -> IO a
loadFirstConfigFileOrElse defConfig filename = do
  (searched, mFile) <- findFirstConfigFile filename
  case mFile of
    Just f ->
      YAML.decodeFileThrow f
    Nothing -> do
      hPutStrLn stderr $ "Configuration file not found: " ++ show filename
      hPutStrLn stderr "Locations searched:"
      forM_ searched $ \s -> do
        hPutStrLn stderr $ "- " ++ show s
      return defConfig

defProviders :: Map Text ProviderFactory
defProviders = [
    ( "default"
    , ProviderFactory $
        const . return $
          localFileProvider (Just "default") "."
    )
  ]

runServer :: IO ()
runServer = do
  defs' <- loadFirstConfigFileOrElse [] "defs.yaml"
  airacs <- loadFirstConfigFileOrElse [] "airac.yaml" :: IO [Airac]
  today <- utctDay <$> getCurrentTime
  let airacMay = findCurrentAiracOn today airacs
  defs <- case airacMay of
    Nothing -> do
      putStrLn "Warning: current AIRAC not found"
      putStrLn "Have:"
      mapM_ (print . Airac.number) airacs
      return defs'
    Just airac -> do
      return $
        Map.fromList
          [ ("airac", JSON.toJSON $ airacDetails airac)
          ]
        <> defs'

  (do
      providerFactories <-
        loadFirstConfigFileOrElse defProviders "providers.yaml"
      let context = defProviderContext { contextDefs = defs }
      providers <- mapM (\factory -> makeProvider factory context) providerFactories

      runServerWith providers
    ) `catch` (\(err :: SomeException) -> putStrLn $ displayException err)
