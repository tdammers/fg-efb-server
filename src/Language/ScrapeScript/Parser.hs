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

parentheses :: Parser a -> Parser a
parentheses = between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

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
    , lamExpr
    , additiveExpr
    ]

letBinding :: Parser (Expr SourcePos -> Expr SourcePos)
letBinding =
  LetE
    <$> getSourcePos
    <*> try (pat <* whitespace <* symbol "<-")
    <*> expr

letExpr :: Parser (Expr SourcePos)
letExpr =
  letBinding <* optional (keyword "in") <*> expr

lamExpr :: Parser (Expr SourcePos)
lamExpr =
  LamE
    <$> getSourcePos
    <*> try (parentheses patList <* symbol "->")
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
    <*> brackets (expr `sepBy` symbol ",")

dictExpr :: Parser (Expr SourcePos)
dictExpr =
  DictE
    <$> getSourcePos
    <*> braces (pair `sepBy` symbol ",")
  where
    pair :: Parser (Expr SourcePos, Expr SourcePos)
    pair = (,) <$> expr <* symbol ":" <*> expr

groupExpr :: Parser (Expr SourcePos)
groupExpr = parentheses expr

caseExpr :: Parser (Expr SourcePos)
caseExpr = do
  p <- getSourcePos
  keyword "case"
  scrutinee <- parentheses expr
  branches <- braces (caseBranch `sepBy` symbol ";")
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
  , parentheses pat
  , listPat
  ]

patList :: Parser (Pat SourcePos)
patList = do
  p <- getSourcePos
  ListP p <$> listItemPat `sepBy` symbol ","

listItemPat :: Parser (ListItemPat SourcePos)
listItemPat = listRemainderItemPat <|> simpleListItemPat

listRemainderItemPat :: Parser (ListItemPat SourcePos)
listRemainderItemPat = try $ do
  p <- getSourcePos
  ListTailPat p <$> option "_" identifier <* symbol "..."

simpleListItemPat :: Parser (ListItemPat SourcePos)
simpleListItemPat = do
  p <- getSourcePos
  basePat <- pat
  optionalMarkerMaybe <- optional (symbol "?")
  case optionalMarkerMaybe of
    Just () ->
      return $ OptionalListItemPat p basePat
    Nothing ->
      return $ RequiredListItemPat p basePat

listPat :: Parser (Pat SourcePos)
listPat = brackets $ do
  p <- getSourcePos
  ListP p <$> listItemPat `sepBy` symbol ","

doExpr :: Parser (Expr SourcePos)
doExpr = do
  p <- getSourcePos
  keyword "do"
  parts <- braces (doPart `sepBy` symbol ";")
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
    [ do { operatorP
         ; rhs <- rhsP
         ; return $ \lhs -> AppE p (LitE p $ BuiltinV operation) (ListE p [ lhs, rhs ])
         }
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
  brackets $ do
    lhs <- expr
    maybe (indexF p lhs) (sliceF p lhs) <$> optional (symbol ":" *> optional expr)
  where
    indexF p lhsE containerE =
      AppE p (LitE p (BuiltinV IndexB)) (ListE p [containerE, lhsE])
    sliceF p lhsE (Just rhsE) containerE =
      AppE p (LitE p (BuiltinV SliceB)) (ListE p [containerE, lhsE, rhsE])
    sliceF p lhsE Nothing containerE =
      AppE p (LitE p (BuiltinV SliceB)) (ListE p [containerE, lhsE])

dotTail :: Parser (Expr SourcePos -> Expr SourcePos)
dotTail = do
  p <- getSourcePos
  key <- symbol "." *> identifier <* whitespace
  return $ \containerE ->
    AppE p
      (LitE p (BuiltinV IndexB))
      (ListE p [containerE, LitE p (StringV key)])

applyTail :: Parser (Expr SourcePos -> Expr SourcePos)
applyTail = do
  p <- getSourcePos
  arg <- ListE p <$> parentheses (expr `sepBy` symbol ",")
  return $ \e -> AppE p e arg

primitiveExpr :: Parser (Expr SourcePos)
primitiveExpr = choice
  [ LitE <$> getSourcePos <*> val
  , VarE <$> getSourcePos <*> identifier <* whitespace
  , groupExpr
  , dictExpr
  , listExpr
  ]
