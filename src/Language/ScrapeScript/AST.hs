module Language.ScrapeScript.AST
where

import qualified Data.Aeson as JSON
import qualified Data.Aeson.KeyMap as JSON
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Text.XML.Cursor as XML
import Data.String (IsString (..))

import FGEFB.URL (URL)

data Expr
  = NullE
    ---- nitty-gritty ----
    -- | A literal
  | LitE !Val
    -- | Variable dereference
  | VarE !Text
    -- | List construct
  | ListE ![Expr]

    -- TODO:
    -- ConcatE Expr Expr
    -- ArithE Operator [Expr]
    -- LookupE Expr Expr
    -- LamE [Text] Expr
    -- AppE Expr [Expr]
    -- CondE [(Expr,Expr)]

    ---- control flow ----
    -- | Chain expressions sequentially
  | DoE ![Expr]
    -- | Variable assignment
  | LetE !Text !Expr !Expr
    -- | For-each (roughly @mapM@)
  | ForEachE !Text !Expr !Expr

    ---- Miscellany ----
    -- | HTTP fetch
  | FetchE !Expr
    -- | Search-and-replace (regex or text)
  | ReplaceE !Expr !Expr !Expr
    -- | Convert text into URL
  | ParseUrlE !Expr

    ---- DOM ----
    -- | DOM query
  | QueryE !Expr !Expr
    -- | Extract text content from XML element
  | XmlTextE !Expr
    -- | Lookup attribute values from XML element
  | XmlAttribE !Expr !Expr
  deriving (Show)

instance Semigroup Expr where
  a <> b = DoE [a, b]

instance Monoid Expr where
  mappend = (<>)
  mempty = NullE

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
    -- TODO: LamV
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
