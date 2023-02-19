{-# LANGUAGE OverloadedStrings #-}

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

data Expr
  = NullE
  | LitE !Val
  | VarE !Text
  | LamE [Text] Expr
  | AppE Expr [Expr]
  | DoE ![Expr]
  | LetE !Text !Expr !Expr
  | ListE ![Expr]
  | DictE ![(Text, Expr)]
  deriving (Show)

isNullE :: Expr -> Bool
isNullE NullE = True
isNullE _ = False

instance Semigroup Expr where
  a <> b = DoE [a, b]

instance Monoid Expr where
  mappend = (<>)
  mempty = NullE

-- | Built-in functions
data Builtin
  = 
  ---- Misc. ----
    IdentB
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
  | ReplaceB
  | MapB
  | FoldB
  | IndexB
  | SliceB
  | KeysB
  | ElemsB

  ---- HTTP ----
  | FetchB
  | ParseUrlB

  ---- DOM ----
  | XmlQueryB
  | XmlTextB
  | XmlAttribB
  deriving (Show, Eq, Ord, Enum, Bounded)

data Val
  = NullV
  | BoolV !Bool
  | StringV !Text
  | IntV !Integer
  | RegexV !Text
  | UrlV !URL
  | XmlV !XML.Cursor
  | ListV ![Val] -- TODO: Use Vector
  | DictV !(Map Text Val)
  | LamV
      !(Map Text Val) -- closure
      ![Text] -- arguments
      !Expr -- body
  | BuiltinV Builtin
  deriving (Show)

instance IsString Val where
  fromString = StringV . Text.pack

valFromJSON :: JSON.Value -> Val
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

stringify :: Val -> Text
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

truthy :: Val -> Bool
truthy NullV = False
truthy (BoolV b) = b
truthy (ListV xs) = not (null xs)
truthy (DictV m) = not (Map.null m)
truthy (IntV i) = i /= 0
truthy (StringV i) = not (Text.null i)
truthy _ = True
