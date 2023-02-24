{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Language.ScrapeScript.Interpreter
where

import Control.Exception
import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.ByteString.Lazy as LBS
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.Text.IO as Text
import Network.HTTP.Types.URI (urlEncode, urlDecode)
import qualified Text.HTML.DOM as HTML
import Text.Megaparsec (SourcePos, sourcePosPretty)
import Text.Printf (printf)
import Text.Read (readMaybe)
import qualified Text.XML as XML
import qualified Text.XML.Cursor as XML

import FGEFB.Regex
import FGEFB.URL (renderURLText, parseURLText, URL (..), normalizeURL)
import FGEFB.Util
import FGEFB.XmlUtil
import FGEFB.HTTP
import Language.ScrapeScript.AST

-- * Runtime errors

data RuntimeError p =
  RuntimeError
    { runtimeErrorLocation :: p
    , runtimeErrorMessage :: String
    }

instance Show (RuntimeError SourcePos) where
  show (RuntimeError p msg) =
    sourcePosPretty p ++ ": " ++ msg

instance Exception (RuntimeError SourcePos) where

-- * Script Contexts

data ScriptContext p =
  ScriptContext
    { scriptVars :: Map Text (Val p)
    , scriptPos :: p
    }
    deriving (Show)

scAssignVar :: Text -> Val p -> ScriptContext p -> ScriptContext p
scAssignVar name val ctx =
  ctx { scriptVars = Map.insert name val (scriptVars ctx) }

scAssignVars :: Map Text (Val p) -> ScriptContext p -> ScriptContext p
scAssignVars vars ctx =
  ctx { scriptVars = vars <> scriptVars ctx }

scLookupVar :: Text -> ScriptContext p -> Maybe (Val p)
scLookupVar name ctx =
  Map.lookup name (scriptVars ctx)

scSetPos :: p -> ScriptContext p -> ScriptContext p
scSetPos p ctx =
  ctx { scriptPos = p }

dictV :: [(Text, Val p)] -> Val p
dictV = DictV . Map.fromList

defVars :: Map Text (Val p)
defVars = Map.fromList
  [ ("ident", BuiltinV IdentB)
  ---- Debugging ----
  , ("Debug", dictV
      [ ("log", BuiltinV DebugLogB)
      ]
    )
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
  , ("match", BuiltinV MatchB)
  , ("map", BuiltinV MapB)
  , ("filter", BuiltinV FilterB)
  , ("fold", BuiltinV FoldB)
  , ("index", BuiltinV IndexB)
  , ("slice", BuiltinV SliceB)
  , ("keys", BuiltinV KeysB)
  , ("elems", BuiltinV ElemsB)
  , ("cons", BuiltinV ConsB)
  , ("snoc", BuiltinV SnocB)

  ---- HTTP ----
  , ("HTTP", dictV
      [ ("get", BuiltinV HttpGetB)
      ]
    )

  ---- URL ----
  , ("URL", dictV
      [ ("parse", BuiltinV ParseUrlB)
      , ("encode", BuiltinV EncodeUrlB)
      ]
    )

  ---- DOM ----
  , ("DOM", dictV
      [ ("query", BuiltinV XmlQueryB)
      , ("text", BuiltinV XmlTextB)
      , ("attrib", BuiltinV XmlAttribB)
      ]
    )
  ]

-- * The Interpreter Monad

type Interpret p = ExceptT (RuntimeError p) (ReaderT (ScriptContext p) IO)

runInterpret :: p -> Map Text (Val p) -> Interpret p a -> IO (Either (RuntimeError p) a)
runInterpret p initialVars a = do
  let ctx = ScriptContext (initialVars <> defVars) p
  runReaderT (runExceptT a) ctx

throwRuntimeError :: String -> Interpret p a
throwRuntimeError msg = do
  p <- asks scriptPos
  throwError $ RuntimeError p msg

throwTypeError :: String -> Val p -> Interpret p a
throwTypeError expected val =
  throwRuntimeError $ printf "Type error: expected %s, but found %s" expected (stringify val)

eitherInterpret :: Either String a -> Interpret p a
eitherInterpret (Left err) = throwRuntimeError err
eitherInterpret (Right a) = return a

readURL :: Text -> Interpret p URL
readURL = eitherInterpret . parseURLText

lookupVar :: Text -> Interpret p (Val p)
lookupVar name = do
  valMay <- asks $ scLookupVar name
  maybe (throwRuntimeError $ "Undefined variable: " ++ show name) return valMay

lookupVarDef :: Val p -> Text -> Interpret p (Val p)
lookupVarDef defVal name = do
  asks $ fromMaybe defVal . scLookupVar name

getVars :: Interpret p (Map Text (Val p))
getVars = asks scriptVars

withVar :: Text -> Val p -> Interpret p a -> Interpret p a
withVar name val = local (scAssignVar name val)

withVars :: Map Text (Val p) -> Interpret p a -> Interpret p a
withVars vars = local (scAssignVars vars)

atPos :: p -> Interpret p a -> Interpret p a
atPos p = local (scSetPos p)

asURL :: Val p -> Interpret p URL
asURL (UrlV url) = return url
asURL x = throwTypeError "URL" x

asXML :: Val p -> Interpret p XML.Node
asXML (XmlV xml) = return xml
asXML x = throwTypeError "XML" x

asString :: Val p -> Interpret p Text
asString (StringV str) = return str
asString x = throwTypeError "string" x

asUrlString :: Val p -> Interpret p Text
asUrlString (StringV str) = return str
asUrlString (UrlV url) = return $ renderURLText url
asUrlString x = throwTypeError "string" x

asInt :: Val p -> Interpret p Int
asInt (StringV str)
  | Just i <- readMaybe (Text.unpack str)
  = return i
asInt (IntV i)
  = return $ fromInteger i
asInt x = throwTypeError "integer" x

asInteger :: Val p -> Interpret p Integer
asInteger (StringV str)
  | Just i <- readMaybe (Text.unpack str)
  = return i
asInteger (IntV i)
  = return i
asInteger x = throwTypeError "integer" x

asList :: Val p -> Interpret p [Val p]
asList (ListV xs) = return xs
asList x = throwTypeError "list" x

asDict :: Val p -> Interpret p (Map Text (Val p))
asDict (DictV xs) = return xs
asDict x = throwTypeError "dictionary" x

asXml :: Val p -> Interpret p XML.Node
asXml (XmlV xml) = return xml
asXml x = throwTypeError "XML" x

lookupMember :: Val p -> Val p -> Interpret p (Val p)
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
              p <- asks scriptPos
              return $
                LamV
                  scope
                  (p <$ listP ["needle", "replacement"])
                  (AppE p (LitE p (BuiltinV ReplaceB))
                    (ListE p [ VarE p "needle", VarE p "replacement", LitE p container ])
                  )
            _ -> return NullV
        x -> throwTypeError "integer or string" x

    UrlV url -> do
      key <- asString keyVal
      case key of
        "protocol" ->
          return $ maybe NullV (StringV . tshow) $ urlProtocol url
        "host" ->
          return $ maybe NullV (StringV . decodeUtf8) $ urlHostName url
        "path" ->
          return $ ListV . map StringV $ urlPath url
        "query" ->
          return $ maybe NullV (DictV . fmap (maybe NullV StringV) . Map.fromList) $ urlQuery url
        "anchor" ->
          return $ maybe NullV (StringV . decodeUtf8) $ urlAnchor url
        "relative" ->
          return $ UrlV url
            { urlProtocol = Nothing
            , urlHostName = Nothing
            }
        _ -> return NullV

    XmlV target -> do
      key <- asString keyVal
      case key of
        "query" -> do
          scope <- asks scriptVars
          p <- asks scriptPos
          return $
            LamV
              scope
              (p <$ listP ["query"])
              (AppE p (LitE p (BuiltinV XmlQueryB))
                (ListE p [ VarE p "query", LitE p container ])
              )
        "attr" -> do
          scope <- asks scriptVars
          p <- asks scriptPos
          return $
            LamV
              scope
              (p <$ listP ["attrName"])
              (AppE p (LitE p (BuiltinV XmlAttribB))
                (ListE p [ LitE p container, VarE p "attrName" ])
              )
        "text" -> do
          return $ StringV $ textContent target
        _ -> return NullV
    x -> throwTypeError "container" x

nth :: [Val p] -> Int -> Interpret p (Val p)
nth args i
  | i < 0
  = throwRuntimeError $ "Negative argument index " ++ show i
  | i < length args
  = return $ args !! i
  | otherwise
  = throwRuntimeError $ "Argument " ++ show i ++ " not given"

nthMay :: [Val p] -> Int -> Interpret p (Maybe (Val p))
nthMay args i
  | i < 0
  = throwRuntimeError $ "Negative argument index " ++ show i
  | i < length args
  = return . Just $ args !! i
  | otherwise
  = return Nothing

-- * Interpreting Statements And Expressions

eval :: Expr p -> Interpret p (Val p)
eval (NullE _p) = return NullV
eval (DoE _p []) = return NullV
eval (DoE p [x]) = atPos p $ eval x
eval (DoE p (x:xs)) = atPos p $ eval x >> eval (DoE p xs)
eval (LetE p pat e body) = atPos p $ do
  val <- eval e
  withPatMatch val pat (eval body)
eval (LitE _p val) = return val
eval (VarE p name) = atPos p $ lookupVar name
eval (ListE p es) =
  atPos p $
  ListV <$> mapM eval es
eval (DictE p pairs) = atPos p $ do
  dictV <$>
    mapM (\(k, v) -> (,) <$> (eval k >>= asString) <*> eval v) pairs
eval (LamE p argsPat body) = atPos p $ do
  closure <- getVars
  return $ LamV closure argsPat body
eval (AppE p fE argE) = atPos p $ do
  f <- eval fE
  arg <- eval argE
  apply f arg
eval (CaseE p scrutineeE branches) = atPos p $ do
  scrutinee <- eval scrutineeE
  evalPatMatches scrutinee branches

evalPatMatches :: Val p -> [(Pat p, [Expr p], Expr p)] -> Interpret p (Val p)
evalPatMatches _ [] =
  return NullV
evalPatMatches val ((pat, guards, expr) : branches) =
  case patMatch val pat of
    Nothing ->
      evalPatMatches val branches
    Just vars ->
      withVars vars $ do
        r <- evalGuarded guards expr
        case r of
          Nothing -> evalPatMatches val branches
          Just x -> return x

evalGuarded :: [Expr p] -> Expr p -> Interpret p (Maybe (Val p))
evalGuarded [] expr =
  Just <$> eval expr
evalGuarded (g:gs) expr = do
  r <- eval g
  if truthy r then
    evalGuarded gs expr
  else
    return Nothing

patMatch :: Val p -> Pat p -> Maybe (Map Text (Val p))
patMatch val (LitP _ val') =
  if val == val' then
    Just Map.empty
  else
    Nothing
patMatch _ (BindP _ "_") =
  Just Map.empty
patMatch val (BindP _ name) =
  Just (Map.singleton name val)
patMatch (ListV valItems) (ListP _ patItems)
  = patMatchListItems valItems patItems
patMatch (DictV valMap) (DictP _ patItems) =
  mconcat <$> forM patItems (\(k, patVal) -> do
      val <- Map.lookup k valMap
      patMatch val patVal
    )
patMatch _ _ =
  Nothing

patMatchListItems :: [Val p] -> [ListItemPat p] -> Maybe (Map Text (Val p))
patMatchListItems [] [] =
  Just Map.empty
patMatchListItems (x:xs) (RequiredListItemPat _ rp : ps) = do
  this <- patMatch x rp
  rest <- patMatchListItems xs ps
  return $ this <> rest
patMatchListItems [] (RequiredListItemPat _ _ : _) =
  Nothing
patMatchListItems (x:xs) (OptionalListItemPat _ rp : ps) =
  case patMatch x rp of
    Nothing ->
      patMatchListItems (x:xs) ps
    Just this -> do
      rest <- patMatchListItems xs ps
      return $ this <> rest
patMatchListItems [] (OptionalListItemPat _ _ : ps) =
  patMatchListItems [] ps
patMatchListItems xs [ListTailPat _ name] =
  Just $ Map.singleton name (ListV xs)
patMatchListItems _ (ListTailPat _ _ : _) =
  Nothing
patMatchListItems (_:_) [] =
  Nothing

withPatMatch :: Val p -> Pat p -> Interpret p a -> Interpret p a
withPatMatch val pat body =
  case patMatch val pat of
    Nothing -> throwRuntimeError $ "Pattern match failed for " ++ Text.unpack (stringify val)
    Just vars -> withVars vars body

apply :: Val p -> Val p -> Interpret p (Val p)
apply f arg =
  case f of
    LamV closure argPat body ->
      withVars closure $
      withPatMatch arg argPat $ 
      eval body

    BuiltinV IdentB ->
      return arg

    BuiltinV DebugLogB -> do
      args <- asList arg
      liftIO $ mapM_ (Text.putStrLn . stringify) $ args
      return arg

    BuiltinV SumB -> do
      operands <- mapM asInteger =<< asList arg
      return $ IntV $ sum operands
    BuiltinV DiffB -> do
      operands <- mapM asInteger =<< asList arg
      case operands of
        [] -> return $ IntV 0
        x:xs -> return $ IntV $ x - sum xs
    BuiltinV ProductB -> do
      operands <- mapM asInteger =<< asList arg
      return $ IntV $ product operands
    BuiltinV QuotientB -> do
      operands <- mapM asInteger =<< asList arg
      case operands of
        [] -> return $ IntV 1
        x:xs -> return $ IntV $ x `div` product xs
    BuiltinV ToStringB -> do
      return $ StringV $ stringify arg
    BuiltinV ToBoolB -> do
      return $ BoolV $ truthy arg
    BuiltinV ToIntB -> do
      maybe NullV IntV . readMaybe . Text.unpack <$> asString arg

    BuiltinV ConcatB -> do
      args <- asList arg
      case args of
        [] -> return NullV
        ListV {} : _ ->
          ListV . concat <$> mapM asList args
        DictV {} : _ ->
          DictV . mconcat <$> mapM asDict args
        StringV {} : _ ->
          StringV . mconcat <$> mapM asString args
        UrlV u : xs ->
          UrlV . foldl (<>) u <$> mapM asURL xs
        (x:_) -> throwTypeError "container or string" x
        
    BuiltinV MapB -> do
      args <- asList arg
      f' <- args `nth` 0
      container <- args `nth` 1
      case container of
        NullV -> return NullV
        ListV xs ->
          ListV <$> mapM (apply f' . ListV . (:[])) xs
        DictV m ->
          dictV <$> mapM (\(k, v) -> (k ,) <$> apply f' (ListV [v])) (Map.toList m)
        x -> throwTypeError "container" x

    BuiltinV FilterB -> do
      args <- asList arg
      f' <- args `nth` 0
      container <- args `nth` 1
      case container of
        NullV -> return NullV
        ListV xs ->
          ListV <$> filterM (fmap truthy . apply f' . ListV . (:[])) xs
        DictV m ->
          dictV <$> filterM (\(k, v) -> truthy <$> apply f' (ListV [v, StringV k])) (Map.toList m)
        x -> throwTypeError "container" x

    BuiltinV FoldB -> do
      args <- asList arg
      f' <- args `nth` 0
      container <- args `nth` 1
      case container of
        NullV ->
          return NullV
        ListV (x:xs) ->
          foldM (\a b -> apply f' (ListV [a, b])) x xs
        ListV [] ->
          return $ ListV []
        x -> throwTypeError "list" x

    BuiltinV KeysB -> do
      args <- asList arg
      container <- args `nth` 0
      case container of
        ListV xs ->
          return $ ListV . map (IntV . fromIntegral) $ [0..length xs - 1]
        DictV m ->
          return $ ListV . map StringV $ Map.keys m
        x -> throwTypeError "container" x

    BuiltinV ElemsB -> do
      args <- asList arg
      container <- args `nth` 0
      case container of
        ListV xs ->
          return $ ListV xs
        DictV m ->
          return $ ListV $ Map.elems m
        x -> throwTypeError "container" x

    BuiltinV ConsB -> do
      args <- asList arg
      x <- args `nth` 0
      xs <- args `nth` 1 >>= asList
      return $ ListV $ x : xs

    BuiltinV SnocB -> do
      args <- asList arg
      xs <- args `nth` 0 >>= asList
      let ys = tail args
      return $ ListV $ xs ++ ys

    BuiltinV IndexB -> do
      args <- asList arg
      container <- args `nth` 0
      index <- args `nth` 1
      lookupMember index container

    BuiltinV SliceB -> do
      args <- asList arg
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

    BuiltinV HttpGetB -> do
      args <- asList arg
      urlVal <- args `nth` 0
      actualURL <- case urlVal of
        UrlV url ->
          return url
        StringV urlText -> do
          case parseURLText urlText of
            Left err -> throwRuntimeError err
            Right url -> return (normalizeURL url)
        x -> throwTypeError "URL or string" x
      rootURL <- lookupVarDef (UrlV mempty) "rootURL" >>= asURL
      fetchHTML (renderURLText $ rootURL <> actualURL)
    BuiltinV ReplaceB -> do
      args <- asList arg
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
    BuiltinV MatchB -> do
      args <- asList arg
      needle <- args `nth` 0
      haystack <- asString =<< args `nth` 1
      case needle of
        RegexV re -> 
          return $ BoolV $ reTest re haystack
        StringV search ->
          return $ BoolV $ search `Text.isInfixOf` haystack
        x ->
          throwTypeError "RegEx or string" x
    BuiltinV ParseUrlB -> do
      args <- asList arg
      url <- asString =<< args `nth` 0
      either throwRuntimeError (return . UrlV) (parseURLText url)
    BuiltinV EncodeUrlB -> do
      args <- asList arg
      str <- asString =<< args `nth` 0
      return $ StringV . decodeUtf8 . urlEncode True . encodeUtf8 $ str
    BuiltinV DecodeUrlB -> do
      args <- asList arg
      str <- asString =<< args `nth` 0
      return $ StringV . decodeUtf8 . urlDecode True . encodeUtf8 $ str
    BuiltinV XmlQueryB -> do
      args <- asList arg
      query <- asString =<< args `nth` 0
      target <- asXml =<< args `nth` 1
      liftIO $
        (ListV . map XmlV <$> evaluate (map XML.node $ jqQuery query (XML.fromNode target)))
        `catch` \(err :: SomeException) -> do
          print err
          return NullV
    BuiltinV XmlTextB -> do
      args <- asList arg
      e <- asXml =<< args `nth` 0
      return $ StringV . textContent $ e
    BuiltinV XmlAttribB -> do
      args <- asList arg
      e <- asXml =<< args `nth` 0
      n <- asString =<< args `nth` 1
      return $ ListV . map StringV $ XML.attribute (uqName n) (XML.fromNode e)

    -- BuiltinV b ->
    --   throwRuntimeError $ "Not implemented: " ++ show b

    x ->
      throwRuntimeError $ "Not a function: " ++ Text.unpack (stringify x)

-- * Workhorses

fetchHTML :: forall p. Text -> Interpret p (Val p)
fetchHTML urlInitial = do
  (url, body) <- liftIO $ httpCachedGET urlInitial
  let doc = HTML.parseLBS body
  return $ 
      dictV
        [ ("url", either (const $ StringV url) UrlV $ parseURLText url)
        , ("dom", XmlV . XML.NodeElement . XML.documentRoot $ doc)
        , ("body", StringV (decodeUtf8 $ LBS.toStrict body))
        ]
