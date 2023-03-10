{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module FGEFB.Providers
where

import FGEFB.Provider
import FGEFB.Providers.GroupProvider
import FGEFB.Providers.LocalFileProvider
import FGEFB.Providers.NavaidJsonProvider
import FGEFB.Providers.HtmlScrapingProvider
import FGEFB.Providers.LuaProvider

import qualified Data.Aeson as JSON
import Data.Aeson ( (.:), (.!=) )
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

newtype ProviderFactory =
  ProviderFactory
    { makeProvider :: ProviderContext -> IO Provider
    }

instance JSON.FromJSON ProviderFactory where
  parseJSON = JSON.withObject "ProviderFactory" goObj
    where
      goObj obj = do
        label <- obj .: "label"
        tag <- obj .: "type"
        case tag :: Text of
          "file" ->
            ProviderFactory . const . return . localFileProvider label <$> obj .: "path"
          "navaid" ->
            ProviderFactory . const . return . navaidJsonProvider label <$> obj .: "template"
          "lua" -> do
            scriptFilename <- obj .: "script"
            return . ProviderFactory $ \context ->
              return $ luaProvider context label scriptFilename
          "html" -> do
            root <- obj .: "url"
            landing <- obj .: "start" .!= "/"
            folderSel <- obj .: "folders"
            documentSel <- obj .: "documents"
            return . ProviderFactory $ \context -> 
              return $ htmlScrapingProvider context label root landing folderSel documentSel

          "group" -> do
            subFactories <- obj .: "providers"
            return . ProviderFactory $ \context -> do
              groupProvider label <$> mapM (flip makeProvider context) subFactories
          _ ->
            fail "Invalid tag"
