{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE OverloadedStrings #-}
module FGEFB.Providers.ScriptedHtmlScrapingProvider
where

import Control.Monad (when, forM)
import Control.Monad.Except
import Control.Monad.State
import Data.Aeson ( (.:), (.:?), (.!=) )
import qualified Data.Aeson as JSON
import qualified Data.Aeson.TH as JSON
import qualified Data.ByteString.Lazy as LBS
import Data.List (foldl', sortOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, listToMaybe, maybeToList, fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.Lazy as LText
import Data.Time
import qualified Data.Vector as Vector
import Debug.Trace (trace, traceShow, traceM, traceShowM)
import Network.HTTP.Base (urlEncode, urlDecode)
import qualified Network.HTTP.Conduit as HTTP
import Network.HTTP.Simple (httpJSON, httpBS)
import qualified Network.HTTP.Simple as HTTP
import qualified Network.HTTP.Types as HTTP
import System.FilePath (takeBaseName, dropExtension, (</>), (<.>))
import Text.Casing as Casing
import qualified Text.HTML.DOM as HTML
import Text.Printf (printf)
import Text.Read (readMaybe)
import qualified Text.XML as XML
import qualified Text.XML.Cursor as XML
import qualified Text.XML.Selectors as XML
import qualified Text.XML.Selectors.Parsers.JQ as XML

import FGEFB.LoadPDF
import FGEFB.Provider
import FGEFB.Regex
import FGEFB.URL (renderURL, parseURL, URL (..), normalizeURL)
import FGEFB.Util

data Statement
  = NullS
  | ThenS Statement Statement
  | ExpS Expr
  | AssignS Text Expr
  deriving (Show)

instance Semigroup Statement where
  (<>) = ThenS

instance Monoid Statement where
  mappend = (<>)
  mempty = NullS

data Expr
  = LitE Val
  | VarE Text
  | FetchE Expr
  | ReplaceE Expr Expr Expr
  | ParseUrlE Expr
  | QueryE Expr Expr
  | StmtE Statement
  | ForEachE Text Expr Expr
  | XmlTextE Expr
  | XmlAttribE Expr Expr
  deriving (Show)

data Val
  = NullV
  | StringV Text
  | RegexV Text
  | UrlV URL
  | XmlV XML.Cursor
  | ListV [Val]
  deriving (Show)

newtype ScriptContext =
  ScriptContext
    { scriptVars :: Map Text Val
    }
    deriving (Show)

scAssignVar :: Text -> Val -> ScriptContext -> ScriptContext
scAssignVar name val ctx =
  ctx { scriptVars = Map.insert name val (scriptVars ctx) }

scLookupVar :: Text -> ScriptContext -> Maybe Val
scLookupVar name ctx =
  Map.lookup name (scriptVars ctx)

type Interpret = ExceptT String (StateT ScriptContext IO)

runInterpret :: Map Text Val -> Statement -> IO (Either String Val)
runInterpret initialVars stmt = do
  let ctx = ScriptContext initialVars
  evalStateT (runExceptT $ interpretS stmt) ctx

withLocalScope :: Interpret a -> Interpret a
withLocalScope action = do
  s <- get
  r <- action
  put s
  return r

throwTypeError :: String -> Val -> Interpret a
throwTypeError expected val =
  throwError $ printf "Type error: expected %s, but found %s" expected (show val)

lookupVar :: Text -> Interpret Val
lookupVar name = do
  valMay <- gets $ scLookupVar name
  maybe (throwError $ "Undefined variable: " ++ show name) return valMay

assignVar :: Text -> Val -> Interpret ()
assignVar name val =
  modify $ scAssignVar name val

asURL :: Val -> Interpret URL
asURL (UrlV url) = return url
asURL x = throwTypeError "URL" x

asString :: Val -> Interpret Text
asString (StringV str) = return str
asString x = throwTypeError "string" x

asList :: Val -> Interpret [Val]
asList (ListV xs) = return xs
asList x = throwTypeError "list" x

asXml :: Val -> Interpret XML.Cursor
asXml (XmlV xml) = return xml
asXml x = throwTypeError "XML" x

interpretS :: Statement -> Interpret Val
interpretS NullS =
  return NullV
interpretS (ThenS a b) = interpretS a >> interpretS b
interpretS (ExpS e) = interpretE e
interpretS (AssignS name e) = do
  val <- interpretE e
  assignVar name val
  return NullV

interpretE :: Expr -> Interpret Val
interpretE (LitE val) = return val
interpretE (VarE name) = lookupVar name
interpretE (FetchE e) = do
  urlVal <- interpretE e
  actualURL <- case urlVal of
    UrlV url ->
      return url
    StringV urlText -> do
      case parseURL urlText of
        Left err -> throwError err
        Right url -> return (normalizeURL url)
    x -> throwTypeError "URL or string" x
  rootURL <- lookupVar "rootURL" >>= asURL
  fetchHTML (renderURL $ rootURL <> actualURL)
interpretE (ReplaceE needleE replacementE haystackE) = do
  needle <- interpretE needleE
  replacement <- asString =<< interpretE replacementE
  haystack <- asString =<< interpretE haystackE
  case needle of
    RegexV re -> 
      return $ StringV $ reReplace re replacement haystack
    StringV search ->
      return $ StringV $ Text.replace search replacement haystack
interpretE (ParseUrlE e) = do
  url <- asString =<< interpretE e
  either throwError (return . UrlV) (parseURL url)
interpretE (QueryE queryE targetE) = do
  query <- asString =<< interpretE queryE
  target <- asXml =<< interpretE targetE
  return $ ListV $ map XmlV $ jqQuery query target
interpretE (StmtE stmt) = interpretS stmt
interpretE (ForEachE iname body xsE) = do
  xs <- asList =<< interpretE xsE
  ListV <$>
    mapM
      (\x -> withLocalScope $ do
        assignVar iname x
        interpretE body
      )
      xs
interpretE (XmlTextE e) = do
  elem <- asXml =<< interpretE e
  return $ StringV . textContent . XML.node $ elem
interpretE (XmlAttribE attrE e) = do
  n <- asString =<< interpretE attrE
  elem <- asXml =<< interpretE e
  return $ ListV . map StringV $ XML.attribute (uqName n) elem

fetchHTML :: Text -> Interpret Val
fetchHTML url =
  XmlV . XML.fromDocument <$> go url 12
  where
    go :: Text -> Int -> Interpret XML.Document
    go url 0 = throwError "Maximum redirect count exceeded"
    go url n = do
      liftIO $ printf "HTTP GET %s\n" url
      rq <- HTTP.parseRequest (Text.unpack url)
      rp <- HTTP.httpLBS rq { HTTP.redirectCount = 0 }
      liftIO $ printf "HTTP %i %s\n"
        (HTTP.getResponseStatusCode rp)
        (decodeUtf8 . HTTP.statusMessage $ HTTP.getResponseStatus rp)
      if HTTP.getResponseStatusCode rp `elem` redirectCodes then do
        let mlocation = lookup "Location" $ HTTP.getResponseHeaders rp
        case mlocation of
          Nothing ->
            throwError "Missing location header"
          Just location -> do
            liftIO $ printf "Redirecting due to Location header: %s\n" (decodeUtf8 location)
            urlLeft <- either throwError return (parseURL url)
            urlRight <- either throwError return (parseURL (decodeUtf8 location))
            let url' = renderURL $ urlLeft <> urlRight
            if url' == url then
              throwError $ "Infinite redirection (location): " <> show url
            else do
              go url' (n - 1)
      else do
        let document = HTML.parseLBS . HTTP.getResponseBody $ rp
            metaRefresh =
              map (combineURLs url) .
              mapMaybe (parseMetaRefresh . mconcat . XML.attribute "content") .
              jqQuery ("meta[http-equiv=Refresh]" :: Text) .
              XML.fromDocument $
              document
        case metaRefresh of
          url':_ -> do
            liftIO $ printf "Redirecting due to meta refresh: %s\n" url'
            if url' == url then
              throwError $ "Infinite redirection (meta refresh): " <> show url
            else
              go url' (n - 1)
          [] -> do
            return document
      where
        redirectCodes = [301, 302, 303, 307, 308]
        parseMetaRefresh :: Text -> Maybe Text
        parseMetaRefresh content =
          let parts = map Text.strip $ Text.splitOn ";" content
          in case parts of
            (_ : url : _) ->
              if "url=" `Text.isPrefixOf` url then
                Just $ Text.drop 4 url
              else
                Nothing
            _ ->
              Nothing


combineURLs :: Text -> Text -> Text
combineURLs current new =
  either error id $ do
    currentURL <- parseURL current
    newURL <- parseURL new
    return $ renderURL (currentURL <> newURL)

uqName :: Text -> XML.Name
uqName t = XML.Name t Nothing Nothing

interpolateVars :: [(Text, Text)] -> Text -> Text
interpolateVars vars template =
  foldl' (flip interpolateVar) template vars

interpolateVar :: (Text, Text) -> Text -> Text
interpolateVar (k, v) =
  Text.replace ("{" <> k <> "}") v

jqQuery :: Text -> XML.Cursor -> [XML.Cursor]
jqQuery q = XML.match $ XML.jqText' q

jqQuery1 :: Text -> XML.Cursor -> Maybe XML.Cursor
jqQuery1 q = listToMaybe . jqQuery q

textContent :: XML.Node -> Text
textContent (XML.NodeContent t) = t
textContent (XML.NodeElement (XML.Element _ _ children)) = mconcat . map textContent $ children
textContent _ = ""
