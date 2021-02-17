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
import System.Environment (getArgs, setEnv)
import Data.Map (Map)
import qualified Data.Map as Map
import System.FilePath ( takeExtension, takeBaseName )
import System.IO.Temp (withSystemTempDirectory)

import FGEFB.Provider
import FGEFB.Providers.LocalFileProvider
import FGEFB.Providers.JsonHttpProvider

captureListing :: Wai.Request -> Maybe [Scotty.Param]
captureListing rq =
  case (filter (not . Text.null) (Wai.pathInfo rq)) of
    [] ->
      Nothing
    providerID:pathItems ->
      if null pathItems then
        Just
          [ ("provider", LText.fromStrict providerID)
          , ("path", "")
          ]
      else if takeExtension (Text.unpack $ last pathItems) == "" then
        Just
          [ ("provider", LText.fromStrict providerID)
          , ("path", LText.fromStrict $ Text.intercalate "/" $ pathItems)
          ]
      else
        Nothing

capturePDF :: Wai.Request -> Maybe [Scotty.Param]
capturePDF rq =
  case (filter (not . Text.null) (Wai.pathInfo rq)) of
    [] ->
      Nothing
    providerID:pathItems ->
      if null pathItems then
        Nothing
      else if takeExtension (Text.unpack $ last pathItems) == ".pdf" then
        Just
          [ ("provider", LText.fromStrict providerID)
          , ("path", LText.fromStrict $ Text.intercalate "/" $ pathItems)
          ]
      else
        Nothing

app :: Map Text Provider -> ScottyM ()
app providers = do
  -- list providers
  Scotty.get "/" $ do
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
        body <- Scotty.liftAndCatchIO $ getPdfPage provider filename page
        Scotty.setHeader "Content-type" "image/jpeg"
        Scotty.raw body

  Scotty.get (Scotty.regex "^.*$") $ do
    -- path <- Scotty.param "0"
    -- Scotty.liftAndCatchIO $ putStrLn path
    Scotty.next

xmlFragmentToDocument :: XML.Element -> XML.Document
xmlFragmentToDocument docroot =
  XML.Document
    { XML.documentPrologue = XML.Prologue [] Nothing []
    , XML.documentEpilogue = []
    , XML.documentRoot = docroot
    }

xmlFileList :: [FileInfo] -> XML.Element
xmlFileList files =
  XML.Element "listing" [] (map xmlFileEntry files)

xmlFileEntry :: FileInfo -> XML.Node
xmlFileEntry info =
    XML.NodeElement $
      XML.Element (if fileType info == Directory then "directory" else "file")
        []
        [ XML.NodeElement $ XML.Element "name" [] [ XML.NodeContent (fileName info) ]
        , XML.NodeElement $ XML.Element "path" [] [ XML.NodeContent (Text.pack $ filePath info) ]
        , XML.NodeElement $ XML.Element "type" [] [ XML.NodeContent (fileTypeString $ fileType info) ]
        ]
    where
      fileTypeString Directory = "dir"
      fileTypeString PDFFile = "pdf"

xmlProviderList :: Map Text a -> XML.Element
xmlProviderList providers =
  XML.Element "listing" [] (map xmlProviderEntry (Map.keys providers))

xmlProviderEntry :: Text -> XML.Node
xmlProviderEntry name =
  xmlFileEntry
    FileInfo
      { fileName = name
      , filePath = "/" ++ Text.unpack name
      , fileType = Directory
      }

runServerWith :: Map Text Provider -> IO ()
runServerWith providers =
  withSystemTempDirectory "fg-efb-cache" $ \tmpdir -> do
    setEnv "PDFCACHE" tmpdir
    scotty 7675 (app providers)

parseProvider :: String -> Maybe (Text, Provider)
parseProvider spec = do
  let parts = Text.splitOn "$" (Text.pack spec)
  case parts of
    [label, "file", rootDir] ->
      return (label, localFileProvider (Text.unpack rootDir))
    [label, "json", urlPattern] ->
      return (label, jsonHttpProvider urlPattern)
    _ ->
      Nothing

parseProvidersIO :: [String] -> IO (Map Text Provider)
parseProvidersIO =
  fmap Map.fromList . mapM (maybe (error "Invalid provider spec") return . parseProvider)

runServer :: [String] -> IO ()
runServer args = parseProvidersIO args >>= runServerWith

main :: IO ()
main = do
  runServer =<< getArgs
