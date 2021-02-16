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
import System.FilePath ( (</>), makeRelative, takeExtension, takeBaseName )
import System.Directory (listDirectory, doesDirectoryExist)
import System.Environment (getArgs)
import Data.Maybe (catMaybes)
import Data.Map (Map)
import qualified Data.Map as Map

data Provider =
  Provider
    { listFiles :: Text -> FilePath -> IO [FileInfo]
    , getPdfPage :: FilePath -> Int -> IO LBS.ByteString
    }

data FileInfo =
  FileInfo
    { fileName :: Text
    , filePath :: FilePath
    , fileType :: FileType
    }
    deriving (Show)

localFileProvider :: FilePath -> Provider
localFileProvider rootDir =
  Provider
    { listFiles = \providerID dirname -> do
        listDirectory(rootDir </> dirname) >>=
          (fmap catMaybes . mapM (classifyFile providerID rootDir dirname))
    , getPdfPage = \filename page -> do
        let cp = (Process.proc "convert"
                  [ "-background", "white"
                  , "-density"
                  , "150"
                  , rootDir </> filename ++ "[" ++ show page ++ "]"
                  , "-quality", "99"
                  , "jpeg:-"
                  ]
                )
                { Process.std_out = Process.CreatePipe
                }
        putStrLn filename
        Process.withCreateProcess cp $ \_ mout _ _ -> do
          case mout of
            Nothing ->
              error "Something bad happened."
            Just out ->
              LBS.fromStrict <$> BS.hGetContents out
    }

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

data FileType
  = Directory
  | PDFFile
  deriving (Show, Eq)

classifyFile :: Text -> FilePath -> FilePath -> FilePath -> IO (Maybe FileInfo)
classifyFile providerID rootdir dirname f = do
  let fullname = rootdir </> dirname </> f
      qname = Text.unpack providerID </> dirname </> f
  doesDirectoryExist fullname >>= \case
    True ->
      return $
        Just FileInfo
          { filePath = qname
          , fileName = Text.pack f
          , fileType = Directory
          }
    False ->
      case takeExtension f of
        ".pdf" ->
          return $
            Just FileInfo
              { filePath = qname
              , fileName = Text.pack $ takeBaseName f
              , fileType = PDFFile
              }
        _ -> return Nothing

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

runServer :: [String] -> IO ()
runServer localProviders = do
  let providers = Map.fromList
        [ ("Local " <> Text.pack (takeBaseName p), localFileProvider p)
        | p <- localProviders
        ]
  scotty 7675 (app providers)

main :: IO ()
main = do
  localProviders <- getArgs
  runServer localProviders
