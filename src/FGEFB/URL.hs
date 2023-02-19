{-# LANGUAGE OverloadedStrings #-}
module FGEFB.URL
where

import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as Text

data Protocol
  = HTTP
  | HTTPS
  deriving (Show, Eq, Ord, Enum, Bounded)

type HostName = Text
type PathItem = Text
type Path = [Text]
type Query = [(Text, Maybe Text)]

data URL =
  URL
    { urlProtocol :: Maybe Protocol
    , urlHostName :: Maybe HostName
    , urlPath :: Path
    , urlQuery :: Maybe Query
    , urlAnchor :: Maybe Text
    }
    deriving (Show, Eq)

instance IsString URL where
  fromString = either error id . parseURL . Text.pack

instance Semigroup URL where
  (<>) = appendURL

instance Monoid URL where
  mempty = URL Nothing Nothing [] Nothing Nothing
  mappend = (<>)

renderURL :: URL -> Text
renderURL url = mconcat
  [ maybe "" renderProtocol $ urlProtocol url
  , maybe "" ("//" <>) $ urlHostName url
  , Text.intercalate "/" $ urlPath url
  , maybe "" (("?" <>) . renderQuery) $ urlQuery url
  , maybe "" ("#" <>) $ urlAnchor url
  ]

renderProtocol :: Protocol -> Text
renderProtocol HTTP = "http:"
renderProtocol HTTPS = "https:"

renderQuery :: Query -> Text
renderQuery q = Text.intercalate "&" $ map renderQueryItem q

renderQueryItem :: (Text, Maybe Text) -> Text
renderQueryItem (k, Nothing) = k
renderQueryItem (k, Just v) = k <> "=" <> v

appendURL :: URL -> URL -> URL
appendURL a b
  | Just _ <- urlProtocol b
  = b
  | Just _ <- urlHostName b
  = b { urlProtocol = urlProtocol a }
  | otherwise
  = a { urlPath = appendPath (urlPath a) (urlPath b)
      , urlQuery = urlQuery b
      , urlAnchor = urlAnchor b
      }

appendPath :: Path -> Path -> Path
appendPath a b
  | isAbsolutePath b
  = b
  | otherwise
  = init a ++ b

isAbsolutePath :: Path -> Bool
isAbsolutePath ("":_) = True
isAbsolutePath _ = False

parseURL :: Text -> Either String URL
parseURL str
  | "//" `Text.isPrefixOf` str
  = parseProtocolRelativeURL str
  | "/" `Text.isPrefixOf` str
  = parseHostRelativeURL str
  | otherwise
  = parseOtherURL str

parseOtherURL :: Text -> Either String URL
parseOtherURL str = do
  let (protoStr, rest) = Text.breakOn ":" str
  if Text.null rest then do
    parseHostRelativeURL str
  else do
    proto <- case protoStr of
      "http" -> return HTTP
      "https" -> return HTTPS
      x -> Left $ "Unknown protocol " ++ show x
    url <- parseProtocolRelativeURL (Text.drop 1 rest)
    return $ url { urlProtocol = Just proto }

parseProtocolRelativeURL :: Text -> Either String URL
parseProtocolRelativeURL str
  | "//" `Text.isPrefixOf` str
  = do
      let (host, rest) = Text.break (`elem` ['/', '?']) $ Text.drop 2 str
      url <- parseHostRelativeURL rest
      return $ url { urlHostName = Just host }
  | otherwise
  = Left $ "Not a protocol-relative URL: " ++ show str

parseHostRelativeURL :: Text -> Either String URL
parseHostRelativeURL str = do
  let (pathStr, queryAnchorStr) = Text.break (`elem` ("#?" :: [Char])) str
      path = Text.splitOn "/" pathStr
      (queryStr, anchorStr) = Text.breakOn "#" queryAnchorStr
      query = parseQuery $ Text.drop 1 queryStr
      anchor = if Text.null anchorStr then
                  Nothing
               else
                Just (Text.drop 1 anchorStr)
  return URL
          { urlProtocol = Nothing
          , urlHostName = Nothing
          , urlPath = path
          , urlQuery = if Text.null queryStr then Nothing else Just query
          , urlAnchor = anchor
          }

parseQuery :: Text -> Query
parseQuery str =
  let items = Text.splitOn "&" str
  in map parseQueryItem items

parseQueryItem :: Text -> (Text, Maybe Text)
parseQueryItem str =
  let (key, rest) = Text.breakOn "=" str
  in (key, if Text.null rest then Nothing else Just (Text.drop 1 rest))

normalizeURL :: URL -> URL
normalizeURL url = url { urlPath = normalizePath (urlPath url) }

normalizePath :: Path -> Path
normalizePath [] = []
normalizePath (".":xs) = normalizePath xs
normalizePath (_:"..":xs) = normalizePath xs
normalizePath (x:xs) = x : normalizePath xs
