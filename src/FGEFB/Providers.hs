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
          "html" -> do
            root <- obj .: "url"
            landing <- obj .: "start" .!= "/"
            folderSel <- obj .: "folders"
            documentSel <- obj .: "documents"
            return . ProviderFactory $ \context -> 
              return $ htmlScrapingProvider context label root landing folderSel documentSel
          "html-scripted" -> do
            root <- obj .: "url"
            folderSpec <- obj .: "folder"
            documentSpec <- obj .: "document"
            return . ProviderFactory $ \context -> do
              (folderFilename, folderSrc) <- case folderSpec of
                c | "@" `Text.isPrefixOf` c -> do
                  let filename = Text.unpack . Text.drop 1 $ c
                  src <- Text.readFile filename
                  return (filename, src)
                c -> do
                  return ("<<inline:folder>>", c)
              folderExpr <- parseExprM "folder" folderSrc
              (documentFilename, documentSrc) <- case documentSpec of
                c | "@" `Text.isPrefixOf` c -> do
                  let filename = Text.unpack . Text.drop 1 $ c
                  src <- Text.readFile filename
                  return (filename, src)
                c -> do
                  return ("<<inline:document>>", c)
              documentExpr <- parseExprM "document" documentSrc
              return $ scriptedHtmlScrapingProvider
                context label root
                folderFilename
                folderSrc
                folderExpr
                documentFilename
                documentSrc
                documentExpr

          "group" -> do
            subFactories <- obj .: "providers"
            return . ProviderFactory $ \context -> do
              groupProvider label <$> mapM (flip makeProvider context) subFactories
          _ ->
            fail "Invalid tag"
