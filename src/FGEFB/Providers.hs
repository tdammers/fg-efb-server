{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module FGEFB.Providers
where

import FGEFB.Provider
import FGEFB.Providers.GroupProvider
import FGEFB.Providers.LocalFileProvider
import FGEFB.Providers.NavaidJsonProvider
import FGEFB.Providers.HtmlScrapingProvider
import FGEFB.Providers.ScriptedHtmlScrapingProvider

import Language.ScrapeScript.Parser

import qualified Data.Aeson as JSON
import Data.Aeson ( (.:), (.!=) )
import Data.Text (Text)

newtype ProviderFactory =
  ProviderFactory
    { makeProvider :: ProviderContext -> Provider
    }

instance JSON.FromJSON ProviderFactory where
  parseJSON = JSON.withObject "ProviderFactory" goObj
    where
      goObj obj = do
        label <- obj .: "label"
        tag <- obj .: "type"
        case tag :: Text of
          "file" ->
            ProviderFactory . const . localFileProvider label <$> obj .: "path"
          "navaid" ->
            ProviderFactory . const . navaidJsonProvider label <$> obj .: "template"
          "html" -> do
            root <- obj .: "url"
            landing <- obj .: "start" .!= "/"
            folderSel <- obj .: "folders"
            documentSel <- obj .: "documents"
            return . ProviderFactory $ \context -> 
              htmlScrapingProvider context label root landing folderSel documentSel
          "html-scripted" -> do
            root <- obj .: "url"
            folderSrc <- obj .: "folder"
            folderExpr <- parseExprM "folder" folderSrc
            documentSrc <- obj .: "document"
            documentExpr <- parseExprM "document" documentSrc
            return . ProviderFactory $ \context ->
              scriptedHtmlScrapingProvider
                context label root
                folderSrc
                folderExpr
                documentSrc
                documentExpr

          "group" -> do
            subFactories <- obj .: "providers"
            return . ProviderFactory $ \context ->
              groupProvider label $ fmap (flip makeProvider context) subFactories
          _ ->
            fail "Invalid tag"
