module FGEFB.Util
where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText

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
