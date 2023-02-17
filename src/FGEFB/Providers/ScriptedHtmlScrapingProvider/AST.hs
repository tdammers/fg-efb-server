module FGEFB.Providers.ScriptedHtmlScrapingProvider.AST
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

-- TODO: Merge Statement and Expr types (?)

data Statement
  = NullS
  | ThenS !Statement !Statement
  | ExpS !Expr
  | AssignS !Text !Expr
  deriving (Show)

instance Semigroup Statement where
  (<>) = ThenS

instance Monoid Statement where
  mappend = (<>)
  mempty = NullS

data Expr
  = ---- nitty-gritty ----
    -- | A literal
    LitE !Val
    -- | Variable dereference
  | VarE !Text
    -- | List construct
  | ListE ![Expr]

    -- TODO:
    -- ConcatE Expr Expr
    -- ArithE Operator [Expr]
    -- FlattenE [Expr]
    -- LookupE Expr Expr
    -- LamE [Text] Expr
    -- AppE Expr [Expr]
    -- CondE [(Expr,Expr)]

    ---- control flow ----
    -- | Run a statement as an expression
  | StmtE !Statement
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

data Val
  = NullV
  | BoolV !Bool
  | StringV !Text
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
