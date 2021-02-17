{-#LANGUAGE TemplateHaskell #-}
{-#LANGUAGE OverloadedStrings #-}
module FGEFB.Providers.JsonHttpProvider
where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Aeson as JSON
import qualified Data.Aeson.TH as JSON
import Network.HTTP.Simple (httpJSON, httpBS)
import qualified Network.HTTP.Simple as HTTP
import Text.Read (readMaybe)
import System.FilePath (takeBaseName, (</>), (<.>))

import FGEFB.Provider
import FGEFB.LoadPDF

data Entry =
  Entry
    { entry_id :: Int
    , entry_parentId :: Int
    , entry_name :: Text
    , entry_title :: Text
    , entry_isDir :: Bool
    , entry_hasChildren :: Bool
    , entry_href :: Maybe Text
    }
    deriving (Show)

JSON.deriveJSON JSON.defaultOptions {JSON.fieldLabelModifier = drop 6} 'Entry

entryToFileInfo :: Text -> Entry -> FileInfo
entryToFileInfo providerID entry =
  FileInfo
    { fileName = entry_title entry
    , filePath =
        Text.unpack providerID </>
          if entry_isDir entry then
            show (entry_id entry)
          else
            show (entry_parentId entry) </> show (entry_id entry) <.> "pdf"
    , fileType =
        if entry_isDir entry then
          Directory
        else
          PDFFile
    }

formatUrl :: Maybe Int -> Text -> Text
formatUrl (Just parentID) pattern =
  Text.replace "{parentid}" (Text.pack $ show parentID) pattern
formatUrl Nothing pattern =
  Text.replace "{parentid}" "" pattern

getEntries :: Maybe Int -> Text -> IO [Entry]
getEntries mparentID pattern = do
  getEntriesFrom (Text.unpack $ formatUrl mparentID pattern)

getEntriesFrom :: String -> IO [Entry]
getEntriesFrom url = do
  rq <- HTTP.parseRequest url
  HTTP.getResponseBody <$> httpJSON rq

jsonHttpProvider :: Maybe Text -> Text -> Provider
jsonHttpProvider mlabel urlPattern =
  Provider
    { label = mlabel
    , listFiles = \providerID dirname -> do
        mparentID <- if dirname == "" then
                      return Nothing
                    else maybe (error "Invalid parentID") (return . Just) $ readMaybe dirname
        entries <- getEntries mparentID urlPattern
        print entries
        return $ map (entryToFileInfo providerID) entries
    , getPdfPage = \filename page -> do
        case Text.splitOn "/" (Text.pack filename) of
          [parentStr, childStr] -> do
            parentID <- maybe (error "Invalid parentID") return . readMaybe . Text.unpack $ parentStr
            childID <- maybe (error "Invalid childID") return . readMaybe . takeBaseName . Text.unpack $ childStr
            entries <- getEntries (Just parentID) urlPattern
            case filter (\e -> entry_id e == childID) entries of
              [Entry { entry_href = Just href }] -> do
                loadPdfPageHttp href page
              _ -> error "Not found"
          _ -> error "Not found"
    }
