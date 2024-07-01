{-# LANGUAGE OverloadedStrings #-}
module FGEFB.Provider
where

import qualified Data.Aeson as JSON
import Data.Bool (bool)
import qualified Data.HashMap.Strict as HashMap
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Vector as Vector

import FGEFB.Util (tshow)

newtype ProviderContext =
  ProviderContext
    { contextDefs :: Map Text JSON.Value
    }
    deriving (Show)

unpackVars :: Map Text JSON.Value -> [(Text, Text)]
unpackVars = unpackVarList "" . Map.toAscList

unpackVarList :: Text -> [(Text, JSON.Value)] -> [(Text, Text)]
unpackVarList prefix = concatMap (unpackVar prefix)

unpackVar :: Text -> (Text, JSON.Value) -> [(Text, Text)]
unpackVar prefix (k, v) = do
  let pk = prefix <> k
  case v of
    JSON.String t -> return (pk, t)
    JSON.Number n -> return (pk, tshow n)
    JSON.Null -> return (pk, "")
    JSON.Bool b -> return (pk, bool "no" "yes" b)
    JSON.Array xs -> do
      let items = Vector.toList xs
      concatMap
        (\(k', v') -> unpackVar "" (pk <> "[" <> k' <> "]", v'))
        (zip (map tshow [0 :: Int ..]) items)
    JSON.Object {} -> do
      o <- case JSON.fromJSON v of
        JSON.Success o -> return o
        x -> error (show x)
      unpackVarList (pk <> ".") (HashMap.toList o)

defProviderContext :: ProviderContext
defProviderContext =
  ProviderContext
    { contextDefs = Map.empty
    }

data Provider =
  Provider
    { label :: Maybe Text
    , listFiles :: [(Text, Text)] -> Text -> Int -> IO FileList
    , getPdf :: [(Text, Text)] -> Text -> IO (Maybe FileDetails)
    }

data FileList =
  FileList
    { fileListFiles :: [FileInfo]
    , fileListMeta :: FileListMeta
    }
    deriving (Show)

data FileListMeta =
  FileListMeta
    { fileListMetaCurrentPage :: Int
    , fileListMetaNumPages :: Int
    , fileListMetaSearchPath :: Maybe Text
    }
    deriving (Show)

data FileDetails =
  FileDetails
    { fileDetailsPath :: FilePath
    , fileDetailsGeorefs :: Map Int FileGeoRef
    }

simpleFileDetails :: FilePath -> FileDetails
simpleFileDetails path = FileDetails path mempty

nullFileListMeta :: FileListMeta
nullFileListMeta = FileListMeta 0 0 Nothing

nullFileList :: FileList
nullFileList = FileList [] nullFileListMeta

setFileListSearchPath :: Text -> FileList -> FileList
setFileListSearchPath path fileList =
  fileList
    { fileListMeta = setFileListMetaSearchPath path $ fileListMeta fileList }

setFileListMetaSearchPath :: Text -> FileListMeta -> FileListMeta
setFileListMetaSearchPath path meta =
  meta { fileListMetaSearchPath = Just path }

paginate :: Int -> [FileInfo] -> FileList
paginate page files =
  FileList
    listedFiles
    meta
  where
    (meta, listedFiles) = paginateRaw page files

paginateRaw :: Int -> [a] -> (FileListMeta, [a])
paginateRaw page files = (meta, listedFiles)
  where
    listedFiles =
      if page < 0 || page >= numPages then
        []
      else
        take perPage . drop (perPage * page) $ files
    perPage = 12
    numPages = (length files + perPage - 1) `div` perPage
    meta = nullFileListMeta
              { fileListMetaCurrentPage = page
              , fileListMetaNumPages = numPages
              }

data FileInfo =
  FileInfo
    { fileName :: Text
    , filePath :: Text
    , fileType :: FileType
    }
    deriving (Show)

fileInfo :: Text -> Text -> FileType -> FileInfo
fileInfo name path ty =
  FileInfo name path ty

defFileInfo :: FileInfo
defFileInfo =
  FileInfo "" "" PDFFile

data FileGeoRef =
  FileGeoRef
    { fileGeoTX :: Double
    , fileGeoTY :: Double
    , fileGeoK :: Double
    , fileGeoTransformAngle :: Double
    , fileGeoPdfPageRotation :: Double
    }
    deriving (Show)

data FileType
  = Directory
  | PDFFile
  deriving (Show, Eq)


