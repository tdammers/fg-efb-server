module FGEFB.Provider
where

import Data.Text (Text)
import System.FilePath (FilePath)
import qualified Data.ByteString.Lazy as LBS
import FGEFB.Airac
import Data.Time (fromGregorian)

data ProviderContext =
  ProviderContext
    { contextAirac :: AiracCycle
    }
    deriving (Show)

defProviderContext :: ProviderContext
defProviderContext =
  ProviderContext
    { contextAirac = AiracCycle 1 (fromGregorian 2000 1 1)
    }

data Provider =
  Provider
    { label :: Maybe Text
    , listFiles :: Text -> FilePath -> IO [FileInfo]
    , getPdfPage :: FilePath -> Int -> IO (Maybe LBS.ByteString)
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


