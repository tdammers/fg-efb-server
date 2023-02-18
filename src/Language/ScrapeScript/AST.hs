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
  | LitE !Val
  | VarE !Text
  | LamE [Text] Expr
  | AppE Expr [Expr]
  | DoE ![Expr]
  | LetE !Text !Expr !Expr
  | ListE ![Expr]
  | DictE ![(Text, Expr)]
  deriving (Show)

instance Semigroup Expr where
  a <> b = DoE [a, b]

instance Monoid Expr where
  mappend = (<>)
  mempty = NullE

-- | Built-in functions
data Builtin
  = IdentB
  ---- Arithmetic ----
  | SumB
  | ProductB
  | DiffB
  | QuotientB

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
