{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE OverloadedLists #-}
{-#LANGUAGE LambdaCase #-}
module Main
where

import qualified Web.Scotty as Scotty
import Web.Scotty (ScottyM, scotty)
import Network.Wai as Wai
import Control.Monad (forM_)
import qualified Text.XML as XML
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Text.Lazy as LText
import Data.Default (def)
import qualified System.Process as Process
import System.Process (CreateProcess)
import System.IO (hPutStrLn, stderr)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import System.Environment (getArgs, setEnv, lookupEnv)
import Data.Map (Map)
import qualified Data.Map as Map
import System.FilePath ( takeExtension, takeBaseName, (</>) )
import System.IO.Temp (withSystemTempDirectory)
import qualified Data.Yaml as YAML
import qualified Data.Aeson as JSON
import Data.Maybe (fromMaybe, catMaybes, listToMaybe)
import Data.Cache (Cache, newCache)
import qualified Data.Cache as Cache
import Data.Hashable (Hashable)
import System.Directory (doesFileExist, XdgDirectory (..), getXdgDirectory, getAppUserDataDirectory)
import Data.Bool (bool)

import FGEFB.Provider
import FGEFB.Providers
import FGEFB.Providers.GroupProvider

captureListing :: Wai.Request -> Maybe [Scotty.Param]
captureListing rq =
  case (filter (not . Text.null) (Wai.pathInfo rq)) of
    "api":pathItems -> Just [("path", LText.fromStrict $ Text.intercalate "/" pathItems)]
    _ -> Nothing

capturePDF :: Wai.Request -> Maybe [Scotty.Param]
capturePDF rq =
  case (filter (not . Text.null) (Wai.pathInfo rq)) of
    "files":pathItems -> Just [("path", LText.fromStrict $ Text.intercalate "/" pathItems)]
    _ -> Nothing

app :: Cache Text [FileInfo] -> Provider -> ScottyM ()
app listingCache provider = do
  Scotty.get "/" $ do
    Scotty.headers >>= Scotty.liftAndCatchIO . print
    Scotty.redirect "/api"

  Scotty.get "/static/style.css" $ do
    Scotty.setHeader "Content-Type" "text/css"
    Scotty.file "./static/style.css"

  Scotty.get "/static/style.xsl" $ do
    Scotty.setHeader "Content-Type" "text/css"
    Scotty.file "./static/style.xsl"

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
      Nothing ->
        Scotty.next
      Just body -> do
        Scotty.setHeader "Content-type" "image/jpeg"
        Scotty.raw body

  Scotty.get (Scotty.regex "^.*$") $ do
    -- path <- Scotty.param "0"
    -- Scotty.liftAndCatchIO $ putStrLn path
    Scotty.next

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

xmlFragmentToDocument :: XML.Element -> XML.Document
xmlFragmentToDocument docroot =
  XML.Document
    { XML.documentPrologue = prologue
    , XML.documentEpilogue = []
    , XML.documentRoot = docroot
    }
    where
      prologue =
        XML.Prologue
          [ XML.MiscInstruction
              XML.Instruction
                { XML.instructionTarget = "xml-stylesheet"
                , XML.instructionData = "type=\"text/xml\" href=\"/static/style.xsl\""
                }
          ]
          Nothing
          []

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
  let candidates = map (</> filename) [xdg, aud]
  found <- mapM (\f -> bool Nothing (Just f) <$> doesFileExist f) candidates
  return (candidates, catMaybes found)

findFirstConfigFile :: FilePath -> IO ([FilePath], Maybe FilePath)
findFirstConfigFile filename = fmap listToMaybe <$> findConfigFiles filename

loadFirstConfigFile :: JSON.FromJSON a => FilePath -> IO a
loadFirstConfigFile = loadFirstConfigFileOrElse (error "Configuration file not found")

loadFirstConfigFileOrElse :: JSON.FromJSON a => a -> FilePath -> IO a
loadFirstConfigFileOrElse def filename = do
  (searched, mFile) <- findFirstConfigFile filename
  case mFile of
    Just f ->
      YAML.decodeFileThrow f
    Nothing -> do
      hPutStrLn stderr $ "Configuration file not found: " ++ show filename
      hPutStrLn stderr $ "Locations searched:"
      forM_ searched $ \s -> do
        hPutStrLn stderr $ "- " ++ show s
      return def

runServer :: IO ()
runServer = do
  defs <- loadFirstConfigFile "defs.yaml"
  providerFactories <- loadFirstConfigFile "providers.yaml"
  let context = defProviderContext { contextDefs = defs }
      providers = fmap (\factory -> makeProvider factory context) providerFactories

  runServerWith providers

main :: IO ()
main = do
  runServer
