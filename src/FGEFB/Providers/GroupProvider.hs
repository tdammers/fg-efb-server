{-# LANGUAGE OverloadedStrings #-}
module FGEFB.Providers.GroupProvider
where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

import FGEFB.Provider

groupProvider :: Maybe Text -> Map Text Provider -> Provider
groupProvider mlabel providers = Provider
  { label = mlabel
  , getPdf = \query path -> do
      case Text.splitOn "/" path of
        [] ->
          return Nothing
        providerID:subpathItems -> do
          let subpath = Text.intercalate "/" subpathItems
          case Map.lookup providerID providers of
            Nothing -> return Nothing
            Just provider -> getPdf provider query subpath
  , listFiles = \query path page -> do
      -- print path
      -- print query
      case Text.splitOn "/" path of
        [] -> return (paginate page $ providerList query)
        [""] -> return (paginate page $ providerList query)
        providerID:subpathItems -> do
          let subpath = Text.intercalate "/" subpathItems
          case Map.lookup providerID providers of
            Nothing ->
              return nullFileList
            Just provider -> do
              FileList infos meta <- listFiles provider query subpath page
              return $
                FileList
                  [ i { filePath = providerID <> "/" <> filePath i }
                  | i <- infos
                  ]
                  (amendMetaSearchPath providerID meta)
  , available = const True
  }
  where
    amendMetaSearchPath providerID meta =
      meta
        { fileListMetaSearchPath = do
            path <- fileListMetaSearchPath meta
            return $ providerID <> "/" <> path
        }
    providerList query =
      [ defFileInfo
          { fileName = fromMaybe providerID $ label provider
          , fileType = Directory
          , filePath = providerID
          }
      | (providerID, provider)
      <- Map.toAscList providers
      , available provider query
      ]
