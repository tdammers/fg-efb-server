{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module FGEFB.Providers.NavaidJsonProvider
where

import qualified Data.Aeson as JSON
import qualified Data.Aeson.TH as JSON
import Data.Text (Text)
import qualified Data.Text as Text
import Network.HTTP.Simple (httpJSON)
import qualified Network.HTTP.Simple as HTTP
import System.FilePath (takeBaseName)
import Text.Read (readMaybe)

import FGEFB.HTTP (downloadHttp)
import FGEFB.Provider
import FGEFB.Util

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

entryToFileInfo :: Entry -> FileInfo
entryToFileInfo entry =
  FileInfo
    { fileName = entry_title entry
    , filePath =
        if entry_isDir entry then
          tshow (entry_id entry)
        else
          tshow (entry_parentId entry) <> "/" <> tshow (entry_id entry) <> ".pdf"
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

navaidJsonProvider :: Maybe Text -> Text -> Provider
navaidJsonProvider mlabel urlPattern =
  Provider
    { label = mlabel
    , listFiles = \_ dirname page -> do
        mparentID <- if dirname == "" then
                      return Nothing
                    else maybe (error "Invalid parentID") (return . Just) $ (readMaybe . Text.unpack) dirname
        entries <- getEntries mparentID urlPattern
        return . paginate page $ map entryToFileInfo entries
    , getPdf = \_ filename -> do
        case Text.splitOn "/" filename of
          [parentStr, childStr] -> do
            parentID <- maybe (error "Invalid parentID") return . readMaybe . Text.unpack $ parentStr
            childID <- maybe (error "Invalid childID") return . readMaybe . takeBaseName . Text.unpack $ childStr
            entries <- getEntries (Just parentID) urlPattern
            case filter (\e -> entry_id e == childID) entries of
              [Entry { entry_href = Just href }] -> do
                Just <$> downloadHttp href ".pdf"
              _ -> error "Not found"
          _ -> error "Not found"
    }
