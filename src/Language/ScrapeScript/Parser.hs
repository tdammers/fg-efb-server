{-# LANGUAGE OverloadedStrings #-}
module Language.ScrapeScript.Parser
where

import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Char

import Language.ScrapeScript.AST

parseExpr :: String -> Text -> Either (ParseErrorBundle Text Void) Expr
parseExpr = runParser (expr <* eof)

type Parser = Parsec Void Text

whitespace :: Parser ()
whitespace = skipMany (space <|> lineComment)

lineComment :: Parser ()
lineComment = void $ string "#" *> takeWhileP Nothing (/= '\n')

symbol :: Text -> Parser Text
symbol = L.symbol whitespace

val :: Parser Val
val = choice
  [ NullV <$ symbol "null"
  , BoolV True <$ symbol "true"
  , BoolV False <$ symbol "false"
  , IntV <$> L.signed whitespace L.decimal <* whitespace
  , StringV <$> stringLiteral <* whitespace
  , RegexV <$> regexLiteral <* whitespace
  ]

stringLiteral :: Parser Text
stringLiteral = fmap Text.pack $ char '"' *> manyTill L.charLiteral (char '"')

regexLiteral :: Parser Text
regexLiteral = fmap Text.pack $
  char '/' *> manyTill (L.charLiteral <|> (char '\\' *> anySingle)) (char '/')

expr :: Parser Expr
expr = choice
  [
  ]

identifier :: Parser Text
identifier =
  Text.cons <$> satisfy isIdentInitial <*> takeWhileP Nothing isIdent

isIdentInitial :: Char -> Bool
isIdentInitial c = isAlpha c || c == '_'

isIdent :: Char -> Bool
isIdent c = isAlphaNum c || c == '_'

listExpr :: Parser Expr
listExpr =
  ListE <$> between (symbol "[") (symbol "]") (expr `sepBy` symbol ",")

doExpr :: Parser Expr
doExpr = do
  symbol "do"
  DoE <$> between (symbol "{") (symbol "}") (expr `sepBy` symbol ";")

primitiveExpr :: Parser Expr
primitiveExpr = choice
  [ LitE <$> val
  , VarE <$> identifier <* whitespace
  , between (symbol "(") (symbol ")") expr
  ]
