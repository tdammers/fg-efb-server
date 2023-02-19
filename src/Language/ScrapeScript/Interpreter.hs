{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Language.ScrapeScript.Interpreter
where

import Control.Exception
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

import FGEFB.Regex
import FGEFB.URL (renderURL, parseURL, URL (..), normalizeURL)
import FGEFB.Util
import FGEFB.XmlUtil
import Language.ScrapeScript.AST

-- * Script Contexts

newtype ScriptContext =
  ScriptContext
    { scriptVars :: Map Text Val
    }
    deriving (Show)

scAssignVar :: Text -> Val -> ScriptContext -> ScriptContext
scAssignVar name val ctx =
  ctx { scriptVars = Map.insert name val (scriptVars ctx) }

scAssignVars :: Map Text Val -> ScriptContext -> ScriptContext
scAssignVars vars ctx =
  ctx { scriptVars = vars <> scriptVars ctx }

scLookupVar :: Text -> ScriptContext -> Maybe Val
scLookupVar name ctx =
  Map.lookup name (scriptVars ctx)

defVars :: Map Text Val
defVars = Map.fromList
  [ ("ident", BuiltinV IdentB)
  ---- Arithmetic ----
  , ("sum", BuiltinV SumB)
  , ("product", BuiltinV ProductB)
  , ("diff", BuiltinV DiffB)
  , ("quotient", BuiltinV QuotientB)

  ---- Coercions ----
  , ("toString", BuiltinV ToStringB)
  , ("toBool", BuiltinV ToBoolB)
  , ("toInt", BuiltinV ToIntB)

  ---- Collections (strings, lists, dictionaries) ----
  , ("concat", BuiltinV ConcatB)
  , ("replace", BuiltinV ReplaceB)
  , ("map", BuiltinV MapB)
  , ("fold", BuiltinV FoldB)
  , ("index", BuiltinV IndexB)
  , ("slice", BuiltinV SliceB)
  , ("keys", BuiltinV KeysB)
  , ("elems", BuiltinV ElemsB)

  ---- HTTP ----
  , ("fetch", BuiltinV FetchB)

  ---- URL ----
  , ("URL", DictV $ Map.fromList
      [ ("parse", BuiltinV ParseUrlB)
      ]
    )

  ---- DOM ----
  , ("DOM", DictV $ Map.fromList
      [ ("query", BuiltinV XmlQueryB)
      , ("text", BuiltinV XmlTextB)
      , ("attrib", BuiltinV XmlAttribB)
      ]
    )
  ]

-- * The Interpreter Monad

type Interpret = ExceptT String (ReaderT ScriptContext IO)

runInterpret :: Map Text Val -> Interpret a -> IO (Either String a)
runInterpret initialVars a = do
  let ctx = ScriptContext (initialVars <> defVars)
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

lookupVarDef :: Val -> Text -> Interpret Val
lookupVarDef defVal name = do
  asks $ fromMaybe defVal . scLookupVar name

getVars :: Interpret (Map Text Val)
getVars = asks scriptVars

withVar :: Text -> Val -> Interpret a -> Interpret a
withVar name val = local (scAssignVar name val)

withVars :: Map Text Val -> Interpret a -> Interpret a
withVars vars = local (scAssignVars vars)

asURL :: Val -> Interpret URL
asURL (UrlV url) = return url
asURL x = throwTypeError "URL" x

asXML :: Val -> Interpret XML.Cursor
asXML (XmlV xml) = return xml
asXML x = throwTypeError "XML" x

asString :: Val -> Interpret Text
asString (StringV str) = return str
asString x = throwTypeError "string" x

asInt :: Val -> Interpret Int
asInt (StringV str)
  | Just i <- readMaybe (Text.unpack str)
  = return i
asInt (IntV i)
  = return $ fromInteger i
asInt x = throwTypeError "integer" x

asInteger :: Val -> Interpret Integer
asInteger (StringV str)
  | Just i <- readMaybe (Text.unpack str)
  = return i
asInteger (IntV i)
  = return i
asInteger x = throwTypeError "integer" x

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
        (x:_) -> return x
        _ -> return NullV
    StringV txt -> do
      case keyVal of
        IntV i ->
          case Text.take 1 $ Text.drop (fromInteger i) txt of
            "" -> return NullV
            t -> return $ StringV t
        StringV key ->
          case key of
            "replace" -> do
              scope <- asks scriptVars
              return $
                LamV
                  scope
                  ["needle", "replacement"]
                  (AppE (LitE (BuiltinV ReplaceB))
                    [ VarE "needle", VarE "replacement", LitE container ]
                  )
            _ -> return NullV
        x -> throwTypeError "integer or string" x

    UrlV url -> do
      key <- asString keyVal
      case key of
        "protocol" ->
          return $ maybe NullV (StringV . tshow) $ urlProtocol url
        "host" ->
          return $ maybe NullV StringV $ urlHostName url
        "path" ->
          return $ ListV . drop 1 . map StringV $ urlPath url
        "query" ->
          return $ maybe NullV (DictV . fmap (maybe NullV StringV) . Map.fromList) $ urlQuery url
        "anchor" ->
          return $ maybe NullV StringV $ urlAnchor url
        _ -> return NullV

    XmlV target -> do
      key <- asString keyVal
      case key of
        "query" -> do
          scope <- asks scriptVars
          return $
            LamV
              scope
              ["query"]
              (AppE (LitE (BuiltinV XmlQueryB))
                [ VarE "query", LitE container ]
              )
        "text" -> do
          let node = XML.node target
          return $ StringV $ textContent node
        _ -> return NullV
    x -> throwTypeError "container" x

nth :: [Val] -> Int -> Interpret Val
nth args i
  | i < 0
  = throwError $ "Negative argument index " ++ show i
  | i < length args
  = return $ args !! i
  | otherwise
  = throwError $ "Argument " ++ show i ++ " not given"

nthMay :: [Val] -> Int -> Interpret (Maybe Val)
nthMay args i
  | i < 0
  = throwError $ "Negative argument index " ++ show i
  | i < length args
  = return . Just $ args !! i
  | otherwise
  = return Nothing

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
eval (ListE es) =
  ListV <$> mapM eval es
eval (DictE pairs) =
  DictV . Map.fromList <$>
    mapM (\(k, v) -> (k ,) <$> eval v) pairs
eval (LamE argNames body) = do
  closure <- getVars
  return $ LamV closure argNames body
eval (AppE fE argEs) = do
  f <- eval fE
  args <- mapM eval argEs
  apply f args

apply :: Val -> [Val] -> Interpret Val
apply f args =
  case f of
    LamV closure argNames body ->
      withVars closure $
      withVars (Map.fromList $ zip argNames args) $
      eval body

    BuiltinV IdentB ->
      args `nth` 0

    BuiltinV SumB -> do
      operands <- mapM asInteger args
      return $ IntV $ sum operands
    BuiltinV DiffB -> do
      operands <- mapM asInteger args
      case operands of
        [] -> return $ IntV 0
        x:xs -> return $ IntV $ x - sum xs
    BuiltinV ProductB -> do
      operands <- mapM asInteger args
      return $ IntV $ product operands
    BuiltinV QuotientB -> do
      operands <- mapM asInteger args
      case operands of
        [] -> return $ IntV 1
        x:xs -> return $ IntV $ x `div` product xs
    BuiltinV ToStringB -> do
      arg <- args `nth` 0
      return $ StringV $ stringify arg
    BuiltinV ToBoolB -> do
      arg <- args `nth` 0
      return $ BoolV $ truthy arg
    BuiltinV ToIntB -> do
      arg <- args `nth` 0
      maybe NullV IntV . readMaybe . Text.unpack <$> asString arg

    BuiltinV ConcatB -> do
      case args of
        [] -> return NullV
        ListV {} : _ ->
          ListV . concat <$> mapM asList args
        DictV {} : _ ->
          DictV . mconcat <$> mapM asDict args
        StringV {} : _ ->
          StringV . mconcat <$> mapM asString args
        UrlV {} : _ ->
          UrlV . mconcat <$> mapM asURL args
        (x:_) -> throwTypeError "container or string" x
        
    BuiltinV MapB -> do
      f' <- args `nth` 0
      container <- args `nth` 1
      case container of
        NullV -> return NullV
        ListV xs ->
          ListV <$> mapM (apply f' . (:[])) xs
        DictV m ->
          DictV . Map.fromList <$> mapM (\(k, v) -> (k ,) <$> apply f' [v]) (Map.toList m)
        x -> throwTypeError "container" x

    BuiltinV FoldB -> do
      f' <- args `nth` 0
      container <- args `nth` 1
      case container of
        NullV ->
          return NullV
        ListV (x:xs) ->
          foldM (\a b -> apply f' [a, b]) x xs
        ListV [] ->
          return $ ListV []
        x -> throwTypeError "list" x

    BuiltinV KeysB -> do
      container <- args `nth` 0
      case container of
        ListV xs ->
          return $ ListV . map (IntV . fromIntegral) $ [0..length xs - 1]
        DictV m ->
          return $ ListV . map StringV $ Map.keys m
        x -> throwTypeError "container" x

    BuiltinV ElemsB -> do
      container <- args `nth` 0
      case container of
        ListV xs ->
          return $ ListV xs
        DictV m ->
          return $ ListV $ Map.elems m
        x -> throwTypeError "container" x

    BuiltinV IndexB -> do
      container <- args `nth` 0
      index <- args `nth` 1
      lookupMember index container

    BuiltinV SliceB -> do
      container <- args `nth` 0
      start <- asInt =<< args `nth` 1
      size <- maybe (pure maxBound) asInt =<< args `nthMay` 2
      case container of
        ListV xs -> do
          let start' = if start < 0 then length xs + start else start
              size' = if size < 0 then length xs - start' + size else size
          return $ ListV $ take size' . drop start' $ xs
        StringV xs -> do
          let start' = if start < 0 then Text.length xs + start else start
              size' = if size < 0 then Text.length xs - start' + size else size
          return $ StringV $ Text.take size' . Text.drop start' $ xs
        x -> throwTypeError "container or string" x

    BuiltinV FetchB -> do
      urlVal <- args `nth` 0
      actualURL <- case urlVal of
        UrlV url ->
          return url
        StringV urlText -> do
          case parseURL urlText of
            Left err -> throwError err
            Right url -> return (normalizeURL url)
        x -> throwTypeError "URL or string" x
      rootURL <- lookupVarDef (UrlV mempty) "rootURL" >>= asURL
      fetchHTML (renderURL $ rootURL <> actualURL)
    BuiltinV ReplaceB -> do
      needle <- args `nth` 0
      replacement <- asString =<< args `nth` 1
      haystack <- asString =<< args `nth` 2
      case needle of
        RegexV re -> 
          return $ StringV $ reReplace re replacement haystack
        StringV search ->
          return $ StringV $ Text.replace search replacement haystack
        x ->
          throwTypeError "RegEx or string" x
    BuiltinV ParseUrlB -> do
      url <- asString =<< args `nth` 0
      either throwError (return . UrlV) (parseURL url)
    BuiltinV XmlQueryB -> do
      query <- asString =<< args `nth` 0
      target <- asXml =<< args `nth` 1
      liftIO $
        (ListV . map XmlV <$> evaluate (jqQuery query target))
        `catch` \(err :: SomeException) -> do
          print err
          return NullV
    BuiltinV XmlTextB -> do
      e <- asXml =<< args `nth` 0
      return $ StringV . textContent . XML.node $ e
    BuiltinV XmlAttribB -> do
      e <- asXml =<< args `nth` 0
      n <- asString =<< args `nth` 1
      return $ ListV . map StringV $ XML.attribute (uqName n) e

    -- BuiltinV b ->
    --   throwError $ "Not implemented: " ++ show b

    x ->
      throwError $ "Not a function: " ++ show x

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
            (_ : redirectUrl : _) ->
              if "url=" `Text.isPrefixOf` redirectUrl then
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
