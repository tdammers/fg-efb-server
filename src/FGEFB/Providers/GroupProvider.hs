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
  , getPdf = \path -> do
      case Text.splitOn "/" path of
        [] ->
          return Nothing
        providerID:subpathItems -> do
          let subpath = Text.intercalate "/" subpathItems
          case Map.lookup providerID providers of
            Nothing -> return Nothing
            Just provider -> getPdf provider subpath
  , listFiles = \path -> do
      case Text.splitOn "/" path of
        [] -> return providerList
        [""] -> return providerList
        providerID:subpathItems -> do
          let subpath = Text.intercalate "/" subpathItems
          case Map.lookup providerID providers of
            Nothing -> return []
            Just provider -> do
              infos <- listFiles provider subpath
              return
                [ i { filePath = providerID <> "/" <> filePath i }
                | i <- infos
                ]
  }
  where
    providerList =
      [ FileInfo
          { fileName = fromMaybe providerID $ label provider
          , fileType = Directory
          , filePath = providerID
          }
      | (providerID, provider)
      <- Map.toAscList providers
      ]
