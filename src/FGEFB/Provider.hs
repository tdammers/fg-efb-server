module FGEFB.Provider
where

import Data.Text (Text)
import System.FilePath (FilePath)
import qualified Data.ByteString.Lazy as LBS

data Provider =
  Provider
    { label :: Maybe Text
    , listFiles :: Text -> FilePath -> IO [FileInfo]
    , getPdfPage :: FilePath -> Int -> IO LBS.ByteString
    }

data FileInfo =
  FileInfo
    { fileName :: Text
    , filePath :: FilePath
    , fileType :: FileType
    }
    deriving (Show)

data FileType
  = Directory
  | PDFFile
  deriving (Show, Eq)


