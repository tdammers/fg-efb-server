{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.ScrapeScript.Interpreter
where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import qualified Network.HTTP.Conduit as HTTP
import qualified Network.HTTP.Simple as HTTP
import qualified Network.HTTP.Types as HTTP
import qualified Text.HTML.DOM as HTML
import Text.Printf (printf)
import Text.Read (readMaybe)
import qualified Text.XML as XML
import qualified Text.XML.Cursor as XML

import FGEFB.LoadPDF
import FGEFB.Provider
import Language.ScrapeScript.AST
import FGEFB.Regex
import FGEFB.URL (renderURL, parseURL, URL (..), normalizeURL)
import FGEFB.XmlUtil

-- * Script Contexts

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

-- * The Interpreter Monad

type Interpret = ExceptT String (ReaderT ScriptContext IO)

runInterpret :: Map Text Val -> Interpret a -> IO (Either String a)
runInterpret initialVars a = do
  let ctx = ScriptContext initialVars
  runReaderT (runExceptT a) ctx

throwTypeError :: String -> Val -> Interpret a
throwTypeError expected val =
  throwError $ printf "Type error: expected %s, but found %s" expected (show val)

eitherInterpret :: Either String a -> Interpret a
eitherInterpret (Left err) = throwError err
eitherInterpret (Right a) = return a

readURL :: Text -> Interpret URL
readURL = eitherInterpret . parseURL

lookupVar :: Text -> Interpret Val
lookupVar name = do
  valMay <- asks $ scLookupVar name
  maybe (throwError $ "Undefined variable: " ++ show name) return valMay


withVar :: Text -> Val -> Interpret a -> Interpret a
withVar name val = local (scAssignVar name val)

asURL :: Val -> Interpret URL
asURL (UrlV url) = return url
asURL x = throwTypeError "URL" x

asString :: Val -> Interpret Text
asString (StringV str) = return str
asString x = throwTypeError "string" x

asInt :: Val -> Interpret Int
asInt (StringV str)
  | Just i <- readMaybe (Text.unpack str)
  = return i
asInt x = throwTypeError "integer" x

asList :: Val -> Interpret [Val]
asList (ListV xs) = return xs
asList x = throwTypeError "list" x

asDict :: Val -> Interpret (Map Text Val)
asDict (DictV xs) = return xs
asDict x = throwTypeError "dictionary" x

asXml :: Val -> Interpret XML.Cursor
asXml (XmlV xml) = return xml
asXml x = throwTypeError "XML" x

lookupMember :: Val -> Val -> Interpret Val
lookupMember keyVal container =
  case container of
    DictV dict -> do
      key <- asString keyVal
      return $ fromMaybe NullV $ Map.lookup key dict
    ListV items -> do
      key <- asInt keyVal
      case drop key items of
        (x:xs) -> return x
        _ -> return NullV
    x -> throwTypeError "list or dictionary" x

-- * Interpreting Statements And Expressions

eval :: Expr -> Interpret Val
eval NullE = return NullV
eval (DoE []) = return NullV
eval (DoE [x]) = eval x
eval (DoE (x:xs)) = eval x >> eval (DoE xs)
eval (LetE name e body) = do
  val <- eval e
  withVar name val $ eval body
eval (LitE val) = return val
eval (VarE name) = lookupVar name
eval (ListE es) = ListV <$> mapM eval es
eval (FetchE e) = do
  urlVal <- eval e
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
eval (ReplaceE needleE replacementE haystackE) = do
  needle <- eval needleE
  replacement <- asString =<< eval replacementE
  haystack <- asString =<< eval haystackE
  case needle of
    RegexV re -> 
      return $ StringV $ reReplace re replacement haystack
    StringV search ->
      return $ StringV $ Text.replace search replacement haystack
    x ->
      throwTypeError "RegEx or string" x
eval (ParseUrlE e) = do
  url <- asString =<< eval e
  either throwError (return . UrlV) (parseURL url)
eval (QueryE queryE targetE) = do
  query <- asString =<< eval queryE
  target <- asXml =<< eval targetE
  return $ ListV $ map XmlV $ jqQuery query target
eval (XmlTextE nodeE) = do
  e <- asXml =<< eval nodeE
  return $ StringV . textContent . XML.node $ e
eval (XmlAttribE attrE nodeE) = do
  n <- asString =<< eval attrE
  e <- asXml =<< eval nodeE
  return $ ListV . map StringV $ XML.attribute (uqName n) e

-- * Workhorses

fetchHTML :: Text -> Interpret Val
fetchHTML url =
  XmlV . XML.fromDocument <$> go url 12
  where
    go :: Text -> Int -> Interpret XML.Document
    go _url 0 = throwError "Maximum redirect count exceeded"
    go _url n = do
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
        metaRefresh <-
          mapM (combineURLs url) .
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

combineURLs :: Text -> Text -> Interpret Text
combineURLs current new = do
  currentURL <- readURL current
  newURL <- readURL new
  return $ renderURL (currentURL <> newURL)
