{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE FlexibleContexts #-}
module FGEFB.Regex
where

import qualified Text.Regex.TDFA as Regex
import Text.Regex.TDFA ( (=~) )
import Data.Text (Text)

reReplace :: Text -> Text -> Text -> Text
reReplace re replacement haystack =
  go haystack
  where
    go "" = ""
    go h = case re =~ h :: (Text, Text, Text) of
      (before, "", "") ->
        before
      (before, _, after) ->
        before <> replacement <> go after

reTest :: Text -> Text -> Bool
reTest re haystack = re =~ haystack
