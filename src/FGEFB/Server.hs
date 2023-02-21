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
import System.Directory (doesFileExist, XdgDirectory (..), getXdgDirectory, getAppUserDataDirectory)
import System.Environment (setEnv, lookupEnv)
import System.FilePath ( (</>) )
import System.IO (hPutStrLn, stderr)
import System.IO.Temp (withSystemTempDirectory)
import qualified Text.XML as XML
import Web.Scotty (ScottyM, scotty)
import qualified Web.Scotty as Scotty

import FGEFB.Airac (Airac, findCurrentAiracOn, airacDetails)
import qualified FGEFB.Airac as Airac
import FGEFB.Provider
import FGEFB.Providers
import FGEFB.Providers.GroupProvider
import FGEFB.Providers.LocalFileProvider
import FGEFB.XmlUtil

captureListing :: Wai.Request -> Maybe [Scotty.Param]
captureListing rq =
  case filter (not . Text.null) (Wai.pathInfo rq) of
    "api":pathItems -> Just [("path", LText.fromStrict $ Text.intercalate "/" pathItems)]
    _ -> Nothing

capturePDF :: Wai.Request -> Maybe [Scotty.Param]
capturePDF rq =
  case filter (not . Text.null) (Wai.pathInfo rq) of
    "files":pathItems -> Just [("path", LText.fromStrict $ Text.intercalate "/" pathItems)]
    _ -> Nothing

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

app :: Cache Text [FileInfo] -> Provider -> ScottyM ()
app listingCache provider = do
  Scotty.defaultHandler $ \err -> do
    Scotty.liftAndCatchIO $ LText.putStrLn err
    Scotty.setHeader "Content-type" "text/xml"
    Scotty.raw . XML.renderLBS def . xmlFragmentToDocument $
      XML.Element
        "error"
        (Map.fromList
          []
        )
        [ XML.NodeContent "Something went wrong." ]
        -- [ XML.NodeContent (LText.toStrict err) ]

  Scotty.get "/" $ do
    Scotty.headers >>= Scotty.liftAndCatchIO . print
    Scotty.redirect "/api"

  Scotty.get "/static/style.css" styleCSS
  Scotty.get "/static/style.xsl" styleXSL
  Scotty.get "/static/error.png" errorPNG
  Scotty.get "/static/notfound.png" notfoundPNG

  -- directory listings
  Scotty.get (Scotty.function captureListing) $ do
    dirname <- Scotty.param "path"
    files <- Scotty.liftAndCatchIO $
      cached listingCache dirname $ listFiles provider dirname
    Scotty.setHeader "Content-type" "text/xml"
    Scotty.raw . XML.renderLBS def . xmlFragmentToDocument $
      xmlFileList files

  -- PDF pages
  Scotty.get (Scotty.function capturePDF) $ do
    filename <- Scotty.param "path"
    page <- Scotty.param "p" `Scotty.rescue` const (return 0)
    mbody <- Scotty.liftAndCatchIO $ getPdfPage provider filename page
    case mbody of
      Nothing -> do
        Scotty.status HTTP.notFound404
        notfoundPNG
      Just body -> do
        Scotty.setHeader "Content-type" "image/jpeg"
        Scotty.raw body

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
    XML.NodeElement $
      XML.Element rootElemName
        []
        [ XML.NodeElement $ XML.Element "name" [] [ XML.NodeContent (fileName info) ]
        , XML.NodeElement $ XML.Element "path" [] [ XML.NodeContent (prefix <> filePath info) ]
        , XML.NodeElement $ XML.Element "type" [] [ XML.NodeContent fileTypeString ]
        ]
    where
      fileTypeString = case fileType info of
        Directory -> "dir"
        PDFFile -> "pdf"
      prefix = case fileType info of
        Directory -> "/api/"
        PDFFile -> "/files/"
      rootElemName = case fileType info of
        Directory -> "directory"
        PDFFile -> "file"

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
  lookupEnv "PDFCACHE" >>= \case
    Nothing -> do
      withSystemTempDirectory "fg-efb-cache" $ \tmpdir -> do
        setEnv "PDFCACHE" tmpdir
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
        const $
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
          providers = fmap (\factory -> makeProvider factory context) providerFactories

      runServerWith providers
    ) `catch` (\(err :: SomeException) -> putStrLn $ displayException err)
