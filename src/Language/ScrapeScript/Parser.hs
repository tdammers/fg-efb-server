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

parseExprM :: MonadFail m => String -> Text -> m (Expr SourcePos)
parseExprM filename src =
  case parseExpr filename src of
    Left err -> fail $ errorBundlePretty err
    Right x -> return x

parseExpr :: String -> Text -> Either (ParseErrorBundle Text Void) (Expr SourcePos)
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

val :: Parser (Val SourcePos)
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

expr :: Parser (Expr SourcePos)
expr = letExpr <|> nonLetExpr

nonLetExpr :: Parser (Expr SourcePos)
nonLetExpr =
  choice
    [ doExpr
    , caseExpr
    , lambdaExpr
    , additiveExpr
    ]

letBinding :: Parser (Expr SourcePos -> Expr SourcePos)
letBinding =
  LetE
    <$> getSourcePos
    <*> try (identifier <* whitespace <* symbol "=")
    <*> expr

letExpr :: Parser (Expr SourcePos)
letExpr =
  letBinding <* keyword "in" <*> expr

lambdaExpr :: Parser (Expr SourcePos)
lambdaExpr =
  LamE
    <$> getSourcePos
    <*> try
          ( between
              (symbol "(") (symbol ")")
              (identifier `sepBy` symbol ",")
            <* symbol "->"
          )
    <*> expr

identifier :: Parser Text
identifier =
  Text.cons <$> satisfy isIdentInitial <*> takeWhileP Nothing isIdent

isIdentInitial :: Char -> Bool
isIdentInitial c = isAlpha c || c == '_'

isIdent :: Char -> Bool
isIdent c = isAlphaNum c || c == '_'

listExpr :: Parser (Expr SourcePos)
listExpr =
  ListE
    <$> getSourcePos
    <*> between (symbol "[") (symbol "]") (expr `sepBy` symbol ",")

dictExpr :: Parser (Expr SourcePos)
dictExpr =
  DictE
    <$> getSourcePos
    <*> between (symbol "{") (symbol "}") (pair `sepBy` symbol ",")
  where
    pair :: Parser (Expr SourcePos, Expr SourcePos)
    pair = (,) <$> expr <* symbol ":" <*> expr

groupExpr :: Parser (Expr SourcePos)
groupExpr = between (symbol "(") (symbol ")") expr

caseExpr :: Parser (Expr SourcePos)
caseExpr = do
  p <- getSourcePos
  keyword "case"
  scrutinee <- between (symbol "(") (symbol ")") expr
  branches <- between (symbol "{") (symbol "}") (caseBranch `sepBy` symbol ";")
  return $ CaseE p scrutinee branches

caseBranch :: Parser (Pat SourcePos, [Expr SourcePos], Expr SourcePos)
caseBranch = do
  branchPat <- pat
  guards <- option [] $ symbol "|" *> expr `sepBy` symbol ","
  symbol "->"
  branchBody <- expr
  return (branchPat, guards, branchBody)

pat :: Parser (Pat SourcePos)
pat = choice
  [ LitP <$> getSourcePos <*> val
  , BindP <$> getSourcePos <*> identifier <* whitespace
  , between (symbol "(") (symbol ")") pat
  , listPat
  ]

listPat :: Parser (Pat SourcePos)
listPat = do
  p <- getSourcePos
  void $ symbol "["
  items <- pat `sepBy` symbol ","
  choice
    [ ListHeadP p items <$ symbol "," <* symbol "..." <* symbol "]"
    , ListP p items <$ symbol "]"
    ]

doExpr :: Parser (Expr SourcePos)
doExpr = do
  p <- getSourcePos
  keyword "do"
  parts <- between (symbol "{") (symbol "}") (doPart `sepBy` symbol ";")
  return $ foldDoParts p parts

data DoPart
  = DoLet (Expr SourcePos -> Expr SourcePos)
  | DoNonLet (Expr SourcePos)

foldDoParts :: SourcePos -> [DoPart] -> Expr SourcePos
foldDoParts p [] =
  NullE p
foldDoParts _ [DoNonLet e] =
  e
foldDoParts p (DoLet binding : xs) =
  binding (foldDoParts p xs)
foldDoParts p xs =
  let (nonlets, lets) = takeNonLets xs
  in DoE p (filter (not . isNullE) $ nonlets ++ [foldDoParts p lets])

takeNonLets :: [DoPart] -> ([Expr SourcePos], [DoPart])
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

binaryExpr :: Parser (Expr SourcePos) -- ^ operand parser
           -> [ (Parser (), Builtin) ] -- ^ (operator, operation)
           -> Parser (Expr SourcePos)
binaryExpr operandP operations = do
  lhs <- operandP
  xs <- many (binaryTail operandP operations)
  return $ foldr ($) lhs (reverse xs)

binaryTail :: Parser (Expr SourcePos)
           -> [ (Parser (), Builtin) ]
           -> Parser (Expr SourcePos -> Expr SourcePos)
binaryTail rhsP operations = do
  p <- getSourcePos
  choice
    [ do { operatorP; rhs <- rhsP; return $ \lhs -> AppE p (LitE p $ BuiltinV operation) [ lhs, rhs ] }
    | (operatorP, operation) <- operations
    ]

additiveExpr :: Parser (Expr SourcePos)
additiveExpr =
  binaryExpr
    multiplicativeExpr
    [ (symbol "+", SumB)
    , (symbol "-", DiffB)
    , (symbol "~", ConcatB)
    ]

multiplicativeExpr :: Parser (Expr SourcePos)
multiplicativeExpr =
  binaryExpr
    applicativeExpr
    [ (symbol "*", ProductB)
    , (symbol "/", QuotientB)
    ]

applicativeExpr :: Parser (Expr SourcePos)
applicativeExpr = do
  lhs <- primitiveExpr
  xs <- many applicativeTail
  return $ foldr ($) lhs (reverse xs)

applicativeTail :: Parser (Expr SourcePos -> Expr SourcePos)
applicativeTail =
  choice
    [ indexTail
    , applyTail
    , dotTail
    ]

indexTail :: Parser (Expr SourcePos -> Expr SourcePos)
indexTail = do
  p <- getSourcePos
  between (symbol "[") (symbol "]") $ do
    lhs <- expr
    maybe (indexF p lhs) (sliceF p lhs) <$> optional (symbol ":" *> optional expr)
  where
    indexF p lhsE containerE =
      AppE p (LitE p (BuiltinV IndexB)) [containerE, lhsE]
    sliceF p lhsE (Just rhsE) containerE =
      AppE p (LitE p (BuiltinV SliceB)) [containerE, lhsE, rhsE]
    sliceF p lhsE Nothing containerE =
      AppE p (LitE p (BuiltinV SliceB)) [containerE, lhsE]

dotTail :: Parser (Expr SourcePos -> Expr SourcePos)
dotTail = do
  p <- getSourcePos
  key <- symbol "." *> identifier <* whitespace
  return $ \containerE ->
    AppE p
      (LitE p (BuiltinV IndexB))
      [containerE, LitE p (StringV key)]

applyTail :: Parser (Expr SourcePos -> Expr SourcePos)
applyTail = do
  p <- getSourcePos
  args <- between (symbol "(") (symbol ")") (expr `sepBy` symbol ",")
  return $ \e -> AppE p e args

primitiveExpr :: Parser (Expr SourcePos)
primitiveExpr = choice
  [ LitE <$> getSourcePos <*> val
  , VarE <$> getSourcePos <*> identifier <* whitespace
  , groupExpr
  , listExpr
  , dictExpr
  ]
