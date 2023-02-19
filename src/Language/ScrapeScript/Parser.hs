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

parseExprM :: MonadFail m => String -> Text -> m Expr
parseExprM filename src =
  case parseExpr filename src of
    Left err -> fail $ errorBundlePretty err
    Right x -> return x

parseExpr :: String -> Text -> Either (ParseErrorBundle Text Void) Expr
parseExpr = runParser (expr <* eof)

type Parser = Parsec Void Text

whitespace :: Parser ()
whitespace = skipMany (space1 <|> lineComment)

lineComment :: Parser ()
lineComment = void $ string "#" *> takeWhileP Nothing (/= '\n')

symbol :: Text -> Parser ()
symbol = void . L.symbol whitespace

keyword :: Text -> Parser ()
keyword kw = void . try $ string kw <* notFollowedBy (satisfy isIdent) <* whitespace

val :: Parser Val
val = choice
  [ NullV <$ keyword "null"
  , BoolV True <$ keyword "true"
  , BoolV False <$ keyword "false"
  , StringV <$> stringLiteral <* whitespace
  , IntV <$> L.signed whitespace L.decimal <* whitespace
  , RegexV <$> regexLiteral <* whitespace
  ]

stringLiteral :: Parser Text
stringLiteral = fmap Text.pack $ char '"' *> manyTill L.charLiteral (char '"')

regexLiteral :: Parser Text
regexLiteral = fmap Text.pack $
  char '/' *> manyTill (L.charLiteral <|> (char '\\' *> anySingle)) (char '/')

expr :: Parser Expr
expr = letExpr <|> nonLetExpr

nonLetExpr :: Parser Expr
nonLetExpr =
  choice
    [ doExpr
    , additiveExpr
    , return NullE
    ]

letBinding :: Parser (Expr -> Expr)
letBinding =
  LetE
    <$> try (identifier <* whitespace <* symbol "=")
    <*> expr

letExpr :: Parser Expr
letExpr =
  letBinding <* keyword "in" <*> expr

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

groupExpr :: Parser Expr
groupExpr = between (symbol "(") (symbol ")") expr

doExpr :: Parser Expr
doExpr = do
  keyword "do"
  parts <- between (symbol "{") (symbol "}") (doPart `sepBy` symbol ";")
  return $ foldDoParts parts

data DoPart
  = DoLet (Expr -> Expr)
  | DoNonLet Expr

foldDoParts :: [DoPart] -> Expr
foldDoParts [] =
  NullE
foldDoParts [DoNonLet e] =
  e
foldDoParts (DoLet binding : xs) =
  binding (foldDoParts xs)
foldDoParts xs =
  let (nonlets, lets) = takeNonLets xs
  in DoE (filter (not . isNullE) $ nonlets ++ [foldDoParts lets])

takeNonLets :: [DoPart] -> ([Expr], [DoPart])
takeNonLets [] = ([], [])
takeNonLets (DoLet binding : xs) = ([], DoLet binding : xs)
takeNonLets (DoNonLet x : xs) =
  let (nonlets, lets) = takeNonLets xs
  in (x : nonlets, lets)

doPart :: Parser DoPart
doPart = doPartLet <|> doPartNonLet

doPartLet :: Parser DoPart
doPartLet =
  DoLet <$> letBinding

doPartNonLet :: Parser DoPart
doPartNonLet =
  DoNonLet <$> nonLetExpr

additiveExpr :: Parser Expr
additiveExpr = do
  lhs <- multiplicativeExpr
  xs <- many additiveTail
  return $ foldr ($) lhs xs

additiveTail :: Parser (Expr -> Expr)
additiveTail =
  choice
    [ do
        symbol "+"
        rhs <- multiplicativeExpr
        return $ \lhs -> AppE (LitE $ BuiltinV SumB) [lhs, rhs]
    , do
        symbol "-"
        rhs <- multiplicativeExpr
        return $ \lhs -> AppE (LitE $ BuiltinV DiffB) [lhs, rhs]
    , do
        symbol "~"
        rhs <- multiplicativeExpr
        return $ \lhs -> AppE (LitE $ BuiltinV ConcatB) [lhs, rhs]
    ]

multiplicativeExpr :: Parser Expr
multiplicativeExpr = do
  lhs <- applicativeExpr
  xs <- many multiplicativeTail
  return $ foldr ($) lhs xs

multiplicativeTail :: Parser (Expr -> Expr)
multiplicativeTail =
  choice
    [ do
        symbol "*"
        rhs <- applicativeExpr
        return $ \lhs -> AppE (LitE $ BuiltinV ProductB) [lhs, rhs]
    , do
        symbol "/"
        rhs <- applicativeExpr
        return $ \lhs -> AppE (LitE $ BuiltinV QuotientB) [lhs, rhs]
    ]

applicativeExpr :: Parser Expr
applicativeExpr = do
  lhs <- primitiveExpr
  xs <- many applicativeTail
  return $ foldr ($) lhs xs

applicativeTail :: Parser (Expr -> Expr)
applicativeTail =
  choice
    [ indexTail
    , applyTail
    ]

indexTail :: Parser (Expr -> Expr)
indexTail = do
  indexE <- between (symbol "[") (symbol "]") expr
  return $ \containerE -> AppE (LitE (BuiltinV IndexB)) [containerE, indexE]

applyTail :: Parser (Expr -> Expr)
applyTail = do
  args <- between (symbol "(") (symbol ")") (expr `sepBy` symbol ",")
  return $ \e -> AppE e args

primitiveExpr :: Parser Expr
primitiveExpr = choice
  [ LitE <$> val
  , VarE <$> identifier <* whitespace
  , groupExpr
  , listExpr
  ]
