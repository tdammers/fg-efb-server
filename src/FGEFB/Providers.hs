{-# LANGUAGE OverloadedStrings #-}
module FGEFB.Providers
where

import FGEFB.Provider
import FGEFB.Providers.LocalFileProvider
import FGEFB.Providers.JsonHttpProvider
import qualified Data.Aeson as JSON
import Data.Aeson ( (.:) )
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
          "json" ->
            jsonHttpProvider label <$> obj .: "template"
          _ ->
            fail "Invalid tag"
