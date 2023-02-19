{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module FGEFB.Regex
where

import Control.Applicative ( (<|>), many )
import Control.Monad (void)
import Data.Array ((!))
import qualified Data.Array as Array
import Data.Char (isDigit)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void (Void)
import qualified Text.Megaparsec as Parsec
import qualified Text.Megaparsec.Char as Parsec
import Text.Regex.TDFA ( Regex, (=~), MatchOffset, matchAll, MatchArray )
import qualified Text.Regex.TDFA as Regex

data Replacement
  = ReplacementLiteral Text
  | ReplacementBackref Int
  deriving (Show)

type Replacements = [Replacement]

parseReplacements :: Text -> Replacements
parseReplacements src =
  either (error . Parsec.errorBundlePretty) id $
    Parsec.parse replacementsP "regex pattern" src

replacementsP :: Parsec.Parsec Void Text Replacements
replacementsP = many replacementP <* Parsec.eof

replacementP :: Parsec.Parsec Void Text Replacement
replacementP = backrefSimpleP <|> escapedP <|> literalP

backrefSimpleP :: Parsec.Parsec Void Text Replacement
backrefSimpleP = do
  void $ Parsec.char '$'
  ReplacementBackref . read . Text.unpack <$> Parsec.takeWhile1P (Just "integer") isDigit

literalP :: Parsec.Parsec Void Text Replacement
literalP =
  ReplacementLiteral <$> Parsec.takeWhile1P Nothing (`notElem` ['\\', '$'])

escapedP :: Parsec.Parsec Void Text Replacement
escapedP = do
  void $ Parsec.char '\\'
  ReplacementLiteral <$> Parsec.takeP Nothing 1

applyReplacements :: Text -> MatchArray -> Replacements -> Text
applyReplacements src ma = mconcat . map (applyReplacement src ma)

applyReplacement :: Text -> MatchArray -> Replacement -> Text
applyReplacement _ _ (ReplacementLiteral t) = t
applyReplacement src ma (ReplacementBackref n) = 
  let (ofs, len) = ma ! n
  in Text.take len . Text.drop ofs $ src

reReplace :: Text -> Text -> Text -> Text
reReplace re replacementSrc haystack =
  let result = go 0 matches
  in -- traceShow matches $
     -- traceShow replacements $
     -- trace (printf "%s /%s/%s/ -> %s" haystack re replacementSrc result) $
     result
  where
    matches :: [MatchArray]
    matches = matchAll (Regex.makeRegex re :: Regex) haystack

    replacements :: Replacements
    replacements = parseReplacements replacementSrc

    go :: MatchOffset -> [MatchArray] -> Text
    go n [] = Text.drop n haystack
    go n (ma:xs)
      | (lo, hi) <- Array.bounds ma
      , lo <= 0 && hi >= 0
      = let (ofs, len) = ma ! 0
            before = Text.take (ofs - n) . Text.drop n $ haystack
            after = go (ofs + len) xs
        in before <> applyReplacements haystack ma replacements <> after
      | otherwise
      = go n xs

reTest :: Text -> Text -> Bool
reTest re haystack =
  haystack =~ re
