{-# LANGUAGE OverloadedStrings #-}
module FGEFB.Provider
where

import Data.Text (Text)
import System.FilePath (FilePath)
import qualified Data.ByteString.Lazy as LBS
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Aeson as JSON
import qualified Data.Vector as Vector
import Data.Bool (bool)

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
    JSON.Object {} -> do
      let JSON.Success o = JSON.fromJSON v
      unpackVarList (pk <> ".") (HashMap.toList o)

defProviderContext :: ProviderContext
defProviderContext =
  ProviderContext
    { contextDefs = Map.empty
    }

data Provider =
  Provider
    { label :: Maybe Text
    , listFiles :: Text -> IO [FileInfo]
    , getPdfPage :: Text -> Int -> IO (Maybe LBS.ByteString)
    }

data FileInfo =
  FileInfo
    { fileName :: Text
    , filePath :: Text
    , fileType :: FileType
    }
    deriving (Show)

data FileType
  = Directory
  | PDFFile
  deriving (Show, Eq)


