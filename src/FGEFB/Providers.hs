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
          "html" ->
            htmlScrapingProvider label
              -- root URL
              <$> obj .: "url"
              -- landing path
              <*> obj .:? "start" .!= "/"

              <*> obj .: "folder-elems"
              <*> obj .:? "folder-elem-url"
              <*> obj .:? "folder-elem-url-attrib"
              <*> obj .:? "folder-elem-label"
              <*> obj .:? "folder-elem-label-attrib"

              <*> obj .: "document-elems"
              <*> obj .:? "document-elem-url"
              <*> obj .:? "document-elem-url-attrib"
              <*> obj .:? "document-elem-label"
              <*> obj .:? "document-elem-label-attrib"
          _ ->
            fail "Invalid tag"
