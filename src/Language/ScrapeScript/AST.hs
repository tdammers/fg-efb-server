{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}

module Language.ScrapeScript.AST
where

import qualified Data.Aeson as JSON
import qualified Data.Aeson.KeyMap as JSON
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import qualified Data.Vector as Vector
import qualified Text.XML as XML
import qualified Text.XML.Cursor as XML
import Data.String (IsString (..))
import Data.Default (def)

import FGEFB.URL (URL, renderURL)
import FGEFB.XmlUtil

data Expr a
  = NullE a
  | LitE a !(Val a)
  | VarE a !Text
  | LamE a [Text] (Expr a)
  | AppE a (Expr a) [Expr a]
  | DoE a ![Expr a]
  | LetE a !Text !(Expr a) !(Expr a)
  | ListE a ![Expr a]
  | DictE a ![(Expr a, Expr a)]
  deriving (Show, Functor)

exprPos :: Expr a -> a
exprPos (NullE p) = p
exprPos (LitE p _) = p
exprPos (VarE p _) = p
exprPos (LamE p _ _) = p
exprPos (AppE p _ _) = p
exprPos (DoE p _) = p
exprPos (LetE p _ _ _) = p
exprPos (ListE p _) = p
exprPos (DictE p _) = p

isNullE :: Expr a -> Bool
isNullE (NullE _) = True
isNullE _ = False

instance Monoid a => Semigroup (Expr a) where
  a <> b = DoE mempty [a, b]

instance Monoid a => Monoid (Expr a) where
  mappend = (<>)
  mempty = NullE mempty

-- | Built-in functions
data Builtin
  = 
  ---- Misc. ----
    IdentB
  | DebugLogB

  ---- Arithmetic ----
  | SumB
  | ProductB
  | DiffB
  | QuotientB

  ---- Coercions ----
  | ToStringB
  | ToBoolB
  | ToIntB

  ---- Collections (strings, lists, dictionaries) ----
  | ConcatB
  | MatchB
  | ReplaceB
  | MapB
  | FoldB
  | IndexB
  | SliceB
  | KeysB
  | ElemsB

  ---- HTTP ----
  | HttpGetB
  | ParseUrlB

  ---- DOM ----
  | XmlQueryB
  | XmlTextB
  | XmlAttribB
  deriving (Show, Eq, Ord, Enum, Bounded)

data Val a
  = NullV
  | BoolV !Bool
  | StringV !Text
  | IntV !Integer
  | RegexV !Text
  | UrlV !URL
  | XmlV !XML.Cursor
  | ListV ![Val a] -- TODO: Use Vector
  | DictV !(Map Text (Val a))
  | LamV
      !(Map Text (Val a)) -- closure
      ![Text] -- arguments
      !(Expr a) -- body
  | BuiltinV Builtin
  deriving (Show, Functor)

instance IsString (Val a) where
  fromString = StringV . Text.pack

valFromJSON :: JSON.Value -> Val a
valFromJSON JSON.Null =
  NullV
valFromJSON (JSON.String t) =
  StringV t
valFromJSON (JSON.Number n) =
  StringV (Text.pack $ show n)
valFromJSON (JSON.Bool b) =
  BoolV b
valFromJSON (JSON.Array a) =
  ListV . map valFromJSON $ Vector.toList a
valFromJSON (JSON.Object a) =
  DictV . fmap valFromJSON . JSON.toMapText $ a

stringify :: Val a -> Text
stringify NullV = "null"
stringify (BoolV True) = "true"
stringify (BoolV False) = "false"
stringify (StringV t) = Text.pack $ show t
stringify (IntV i) = Text.pack $ show i
stringify (RegexV r) = "/" <> r <> "/"
stringify (UrlV u) = renderURL u
stringify (ListV xs) =
  "[" <> Text.intercalate ", " (map stringify xs) <> "]"
stringify (DictV pairs) =
  "{" <> Text.intercalate ", " [ k <> ": " <> stringify v | (k, v) <- Map.toList pairs ] <> "}"
stringify (LamV {}) =
  "<<lambda>>"
stringify (BuiltinV b) =
  "<<builtin:" <> (Text.dropEnd 1 . Text.pack $ show b) <> ">>"
stringify (XmlV cursor) =
  case XML.node cursor of
    XML.NodeElement e ->
      let doc = xmlFragmentToDocumentNoPrologue e
      in LText.toStrict $
            XML.renderText
              def
                { XML.rsXMLDeclaration = False
                , XML.rsPretty = True
                }
              doc
    _ ->
      "<!-- XML -->"

truthy :: Val a -> Bool
truthy NullV = False
truthy (BoolV b) = b
truthy (ListV xs) = not (null xs)
truthy (DictV m) = not (Map.null m)
truthy (IntV i) = i /= 0
truthy (StringV i) = not (Text.null i)
truthy _ = True
