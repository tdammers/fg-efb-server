{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE FlexibleContexts #-}
module FGEFB.Regex
where

import qualified Text.Regex.TDFA as Regex
import Text.Regex.TDFA ( (=~), MatchOffset, MatchLength, getAllMatches )
import qualified Data.Text as Text
import Data.Text (Text)
import Debug.Trace

reReplace :: Text -> Text -> Text -> Text
reReplace re replacement haystack =
  go 0 matches
  where
    matches :: [(MatchOffset, MatchLength)]
    matches = getAllMatches (haystack =~ re) :: [(MatchOffset, MatchLength)]

    go :: MatchOffset -> [(MatchOffset, MatchLength)] -> Text
    go n [] = Text.drop n haystack
    go n ((ofs, len):xs) =
      let before = Text.take (ofs - n) . Text.drop n $ haystack
          after = go (ofs + len) xs
      in before <> replacement <> after

reTest :: Text -> Text -> Bool
reTest re haystack = haystack =~ re
