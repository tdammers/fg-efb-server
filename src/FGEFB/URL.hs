{-# LANGUAGE OverloadedStrings #-}
module FGEFB.URL
where

import Data.Binary.Builder (Builder)
import qualified Data.Binary.Builder as Builder
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Char
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Word
import Network.HTTP.Types.URI
import Data.Maybe (isNothing)

ordB :: Char -> Word8
ordB = fromIntegral . ord

data Protocol
  = HTTP
  | HTTPS
  deriving (Show, Eq, Ord, Enum, Bounded)

type HostName = ByteString
type PathItem = Text
type Path = [PathItem]

data URL =
  URL
    { urlProtocol :: Maybe Protocol
    , urlHostName :: Maybe HostName
    , urlPath :: Path
    , urlQuery :: Maybe QueryText
    , urlAnchor :: Maybe ByteString
    }
    deriving (Show, Eq)

instance IsString URL where
  fromString = either error id . parseURLText . Text.pack

instance Semigroup URL where
  (<>) = appendURL

instance Monoid URL where
  mempty = URL Nothing Nothing [] Nothing Nothing
  mappend = (<>)

renderURL :: URL -> ByteString
renderURL url = LBS.toStrict . Builder.toLazyByteString $ mconcat
  [ maybe "" renderProtocolBuilder $ urlProtocol url
  , maybe "" (("//" <>) . Builder.fromByteString) $ urlHostName url
  , if isNothing (urlProtocol url) && isNothing (urlHostName url) then
      encodePathSegmentsRelative $ urlPath url
    else
      encodePathSegments . dropWhile Text.null $ urlPath url
  , maybe "" (renderQueryBuilder True . queryTextToQuery) $ urlQuery url
  , maybe "" (("#" <>) . Builder.fromByteString) $ urlAnchor url
  ]

renderURLText :: URL -> Text
renderURLText = decodeUtf8 . renderURL

renderProtocolBuilder :: Protocol -> Builder
renderProtocolBuilder HTTP = "http:"
renderProtocolBuilder HTTPS = "https:"

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
  | null a
  = b
  | otherwise
  = init a ++ b

isAbsolutePath :: Path -> Bool
isAbsolutePath ("":_) = True
isAbsolutePath _ = False

appendQueryMay :: Maybe QueryText -> URL -> URL
appendQueryMay q url =
  url
    { urlQuery = urlQuery url <> q
    }

appendQuery :: QueryText -> URL -> URL
appendQuery = appendQueryMay . Just

parseURL :: ByteString -> Either String URL
parseURL str
  | "//" `BS.isPrefixOf` str
  = parseProtocolRelativeURL str
  | "/" `BS.isPrefixOf` str
  = parseHostRelativeURL str
  | otherwise
  = parseOtherURL str

parseURLText :: Text -> Either String URL
parseURLText = parseURL . encodeUtf8

parseOtherURL :: ByteString -> Either String URL
parseOtherURL str = do
  let (protoStr, rest) = BS.break (== ordB ':') str
  if BS.null rest then do
    parseHostRelativeURL str
  else do
    proto <- case protoStr of
      "http" -> return HTTP
      "https" -> return HTTPS
      x -> Left $ "Unknown protocol " ++ show x
    url <- parseProtocolRelativeURL (BS.drop 1 rest)
    return $ url { urlProtocol = Just proto }

parseProtocolRelativeURL :: ByteString -> Either String URL
parseProtocolRelativeURL str
  | "//" `BS.isPrefixOf` str
  = do
      let (host, rest) = BS.break (`elem` [ordB '/', ordB '?']) $ BS.drop 2 str
      url <- parseHostRelativeURL rest
      return $ url { urlHostName = Just host }
  | otherwise
  = Left $ "Not a protocol-relative URL: " ++ show str

parseHostRelativeURL :: ByteString -> Either String URL
parseHostRelativeURL str = do
  let (pathStr, queryAnchorStr) = BS.break (`elem` map ordB "#?") str
      path = case pathStr of
        "" -> []
        "/" -> [""]
        _ | "/" `BS.isPrefixOf` pathStr -> "" : decodePathSegments pathStr
        _ -> decodePathSegments pathStr
      (queryStr, anchorStr) = BS.break (== ordB '#') queryAnchorStr
      query = parseQueryText $ BS.drop 1 queryStr
      anchor = if BS.null anchorStr then
                  Nothing
               else
                Just (BS.drop 1 anchorStr)
  return URL
          { urlProtocol = Nothing
          , urlHostName = Nothing
          , urlPath = path
          , urlQuery = if BS.null queryStr then Nothing else Just query
          , urlAnchor = anchor
          }

normalizeURL :: URL -> URL
normalizeURL url = url { urlPath = normalizePath (urlPath url) }

normalizePath :: Path -> Path
normalizePath [] = []
normalizePath (".":xs) = normalizePath xs
normalizePath (_:"..":xs) = normalizePath xs
normalizePath (x:xs) = x : normalizePath xs
