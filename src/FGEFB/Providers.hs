{-# LANGUAGE OverloadedStrings #-}
module FGEFB.Providers
where

import FGEFB.Provider
import FGEFB.Providers.LocalFileProvider
import FGEFB.Providers.NavaidJsonProvider
import FGEFB.Providers.HtmlScrapingProvider

import qualified Data.Aeson as JSON
import Data.Aeson ( (.:), (.:?), (.!=) )
import Data.Text (Text)
import qualified Data.Text as Text

instance JSON.FromJSON Provider where
  parseJSON x = JSON.withObject "Provider" goObj x
    where
      goObj obj = do
        label <- obj .: "label"
        tag <- obj .: "type"
        case tag :: Text of
          "file" ->
            localFileProvider label <$> obj .: "path"
          "navaid" ->
            navaidJsonProvider label <$> obj .: "template"
          "html" -> do
            root <- obj .: "url"
            landing <- obj .: "start" .!= "/"
            folderSel <- obj .: "folder"
            documentSel <- obj .: "document"
            return $
              htmlScrapingProvider label root landing folderSel documentSel
          _ ->
            fail "Invalid tag"
