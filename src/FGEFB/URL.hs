{-#LANGUAGE OverloadedStrings #-}
module FGEFB.URL
where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.String (IsString (..))

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
    }
    deriving (Show, Eq)

instance IsString URL where
  fromString = either error id . parseURL . Text.pack

instance Semigroup URL where
  (<>) = appendURL

renderURL :: URL -> Text
renderURL url = mconcat
  [ case urlProtocol url of
      Nothing -> ""
      Just proto -> renderProtocol proto
  , case urlHostName url of
      Nothing -> ""
      Just host -> "//" <> host
  , Text.intercalate "/" $ urlPath url
  , case urlQuery url of
      Nothing -> ""
      Just query -> "?" <> renderQuery query
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
  let (protoStr, rem) = Text.breakOn ":" str
  if Text.null rem then do
    parseHostRelativeURL str
  else do
    proto <- case protoStr of
      "http" -> return HTTP
      "https" -> return HTTPS
      x -> Left $ "Unknown protocol " ++ show x
    url <- parseProtocolRelativeURL (Text.drop 1 rem)
    return $ url { urlProtocol = Just proto }

parseProtocolRelativeURL :: Text -> Either String URL
parseProtocolRelativeURL str
  | "//" `Text.isPrefixOf` str
  = do
      let (host, rem) = Text.break (`elem` ['/', '?']) $ Text.drop 2 str
      url <- parseHostRelativeURL rem
      return $ url { urlHostName = Just host }
  | otherwise
  = Left $ "Not a protocol-relative URL: " ++ show str

parseHostRelativeURL :: Text -> Either String URL
parseHostRelativeURL str = do
  let (pathStr, queryStr) = Text.breakOn "?" str
      path = Text.splitOn "/" pathStr
      query = parseQuery $ Text.drop 1 queryStr
  return URL
          { urlProtocol = Nothing
          , urlHostName = Nothing
          , urlPath = path
          , urlQuery = if Text.null queryStr then Nothing else Just query
          }

parseQuery :: Text -> Query
parseQuery str =
  let items = Text.splitOn "&" str
  in map parseQueryItem items

parseQueryItem :: Text -> (Text, Maybe Text)
parseQueryItem str =
  let (key, rem) = Text.breakOn "=" str
  in (key, if Text.null rem then Nothing else Just (Text.drop 1 rem))
