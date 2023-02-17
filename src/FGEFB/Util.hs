{-# LANGUAGE OverloadedStrings #-}

module FGEFB.Util
where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import Data.List (foldl')

import qualified System.FilePath as FilePath



tshow :: Show a => a -> Text
tshow = Text.pack . show

withString :: StringLike s => (String -> String) -> s -> s
withString f = fromString . f . toString

class StringLike s where
  toString :: s -> String
  fromString :: String -> s

instance StringLike Text where
  toString = Text.unpack
  fromString = Text.pack

instance StringLike LText.Text where
  toString = LText.unpack
  fromString = LText.pack

interpolateVars :: [(Text, Text)] -> Text -> Text
interpolateVars vars template =
  foldl' (flip interpolateVar) template vars

interpolateVar :: (Text, Text) -> Text -> Text
interpolateVar (k, v) =
  Text.replace ("{" <> k <> "}") v

