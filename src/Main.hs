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
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import System.Environment (getArgs, setEnv, lookupEnv)
import Data.Map (Map)
import qualified Data.Map as Map
import System.FilePath ( takeExtension, takeBaseName, (</>) )
import System.IO.Temp (withSystemTempDirectory)
import qualified Data.Yaml as YAML
import Data.Maybe (fromMaybe)

import FGEFB.Provider
import FGEFB.Providers
import FGEFB.Airac

captureListing :: Wai.Request -> Maybe [Scotty.Param]
captureListing rq =
  case (filter (not . Text.null) (Wai.pathInfo rq)) of
    "api":providerID:pathItems ->
      if null pathItems then
        Just
          [ ("provider", LText.fromStrict providerID)
          , ("path", "")
          ]
      else if takeExtension (Text.unpack $ last pathItems) /= ".pdf" then
        Just
          [ ("provider", LText.fromStrict providerID)
          , ("path", LText.fromStrict $ Text.intercalate "/" $ pathItems)
          ]
      else
        Nothing
    _ -> Nothing

capturePDF :: Wai.Request -> Maybe [Scotty.Param]
capturePDF rq =
  case (filter (not . Text.null) (Wai.pathInfo rq)) of
    "files":providerID:pathItems ->
      if null pathItems then
        Nothing
      else if takeExtension (Text.unpack $ last pathItems) == ".pdf" then
        Just
          [ ("provider", LText.fromStrict providerID)
          , ("path", LText.fromStrict $ Text.intercalate "/" $ pathItems)
          ]
      else
        Nothing
    _ -> Nothing

app :: Map Text Provider -> ScottyM ()
app providers = do
  Scotty.get "/" $ do
    Scotty.headers >>= Scotty.liftAndCatchIO . print
    Scotty.redirect "/api"

  Scotty.get "/static/style.css" $ do
    Scotty.setHeader "Content-Type" "text/css"
    Scotty.file "./static/style.css"

  Scotty.get "/static/style.xsl" $ do
    Scotty.setHeader "Content-Type" "text/css"
    Scotty.file "./static/style.xsl"

  -- list providers
  Scotty.get "/api" $ do
    Scotty.setHeader "Content-type" "text/xml"
    Scotty.raw . XML.renderLBS def . xmlFragmentToDocument $ xmlProviderList providers
    
  -- directory listings
  Scotty.get (Scotty.function captureListing) $ do
    providerID <- Scotty.param "provider"
    dirname <- Scotty.param "path"
    case Map.lookup providerID providers of
      Nothing -> Scotty.next
      Just provider -> do
        files <- Scotty.liftAndCatchIO $ listFiles provider providerID dirname
        Scotty.setHeader "Content-type" "text/xml"
        Scotty.raw . XML.renderLBS def . xmlFragmentToDocument $ xmlFileList files

  -- PDF pages
  Scotty.get (Scotty.function capturePDF) $ do
    providerID <- Scotty.param "provider"
    filename <- Scotty.param "path"
    case Map.lookup providerID providers of
      Nothing -> Scotty.next
      Just provider -> do
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
        , XML.NodeElement $ XML.Element "path" [] [ XML.NodeContent (Text.pack $ prefix </> filePath info) ]
        , XML.NodeElement $ XML.Element "type" [] [ XML.NodeContent fileTypeString ]
        ]
    where
      fileTypeString = case fileType info of
        Directory -> "dir"
        PDFFile -> "pdf"
      prefix = case fileType info of
        Directory -> "/api"
        PDFFile -> "/files"
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
      , filePath = Text.unpack key
      , fileType = Directory
      }

runServerWith :: Map Text Provider -> IO ()
runServerWith providers =
  lookupEnv "PDFCACHE" >>= \case
    Nothing -> do
      withSystemTempDirectory "fg-efb-cache" $ \tmpdir -> do
        setEnv "PDFCACHE" tmpdir
        run
    Just _ -> do
      run
  where
    run = scotty 7675 (app providers)

runServer :: IO ()
runServer = do
  airac <- YAML.decodeFileThrow "./airac.yaml"
  providerFactories <- YAML.decodeFileThrow "./providers.yaml"
  let context = defProviderContext { contextAirac = airac }
      providers = fmap (\factory -> makeProvider factory context) providerFactories

  runServerWith providers

main :: IO ()
main = do
  runServer
