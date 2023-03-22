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
    , listFiles :: Text -> IO [FileInfo]
    , getPdf :: Text -> IO (Maybe FilePath)
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


