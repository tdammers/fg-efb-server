{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module FGEFB.HTTP
where

import Control.Exception (Exception, throwIO)
import qualified Crypto.Hash as Crypto
import Crypto.Hash.Algorithms (SHA512)
import Data.Binary (Binary)
import qualified Data.Binary as Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Network.HTTP.Conduit as HTTP
import qualified Network.HTTP.Simple as HTTP
import qualified Network.HTTP.Types as HTTP
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import System.FilePath ( (</>), (<.>) )
import System.IO.Temp (getCanonicalTemporaryDirectory)
import qualified Text.HTML.DOM as HTML
import Text.Printf (printf)
import qualified Text.XML.Cursor as XML

import FGEFB.URL
import FGEFB.XmlUtil (jqQuery)

data HttpError = HttpError (Maybe Int) String
  deriving (Show)

instance Exception HttpError where

throwHTTPError :: String -> IO a
throwHTTPError = throwIO . HttpError Nothing

throwHTTPStatusError :: Int -> String -> IO a
throwHTTPStatusError code msg = throwIO $ HttpError (Just code) msg

getCacheDir :: IO FilePath
getCacheDir = do
  lookupEnv "CACHE_DIR" >>= \case
    Nothing -> getCanonicalTemporaryDirectory
    Just dir -> return dir

getCacheFilename :: Text -> FilePath -> IO FilePath
getCacheFilename url extension = do
  tmpdir <- getCacheDir
  let basename = show (Crypto.hash (encodeUtf8 url) :: Crypto.Digest SHA512)
  return $ tmpdir </> ("fgefb-cache-" ++  basename) <.> extension

withCache :: Text -> FilePath -> (Text -> FilePath -> IO ()) -> IO FilePath
withCache url extension download = do
  printf "Requested: %s\n" url
  filename <- getCacheFilename url extension
  doesFileExist filename >>= \case
    False -> do
      printf "%s not found, downloading from %s\n" filename url
      download url filename
    _ -> do
      printf "%s found\n" filename
  return filename

withCacheImmediate :: Binary a => Text -> (Text -> IO a) -> IO a
withCacheImmediate url download = do
  printf "Requested: %s\n" url
  filename <- getCacheFilename url ".raw"
  doesFileExist filename >>= \case
    False -> do
      printf "%s not found, downloading from %s\n" filename url
      val <- download url
      Binary.encodeFile filename val
      return val
    _ -> do
      printf "%s found\n" filename
      Binary.decodeFile filename

downloadHttp :: Text -> FilePath -> IO FilePath
downloadHttp url extension = do
  withCache url extension $ \url' filename -> do
    rq <- HTTP.parseRequest (Text.unpack url')
    printf "HTTP %s -> %s\n" url filename
    rp <- HTTP.httpBS rq
    BS.writeFile filename (HTTP.getResponseBody rp)

httpCachedGET :: Text -> IO (Text, LBS.ByteString)
httpCachedGET url = do
  withCacheImmediate url $ httpGET

httpGET :: Text -> IO (Text, LBS.ByteString)
httpGET urlInitial = do
  go urlInitial 12

  where
    go :: Text -> Int -> IO (Text, LBS.ByteString)
    go _url 0 =
      throwHTTPError "Maximum redirect count exceeded"
    go url n = do
      printf "HTTP GET %s\n" url
      rq <- HTTP.parseRequest (Text.unpack url)
      rp <- HTTP.httpLBS rq { HTTP.redirectCount = 0 }
      printf "HTTP %i %s\n"
        (HTTP.getResponseStatusCode rp)
        (decodeUtf8 . HTTP.statusMessage $ HTTP.getResponseStatus rp)
      if HTTP.getResponseStatusCode rp `elem` redirectCodes then do
        let mlocation = lookup "Location" $ HTTP.getResponseHeaders rp
        case mlocation of
          Nothing ->
            throwHTTPError "Missing location header"
          Just location -> do
            printf "Redirecting due to Location header: %s\n" (decodeUtf8 location)
            urlLeft <- either throwHTTPError return (parseURLText url)
            urlRight <- either throwHTTPError return (parseURLText (decodeUtf8 location))
            let url' = renderURLText $ urlLeft <> urlRight
            if url' == url then
              throwHTTPError $ "Infinite redirection (location): " <> show url
            else do
              go url' (n - 1)
      else do
        let contentTypeHeaders = HTTP.getResponseHeader "Content-Type" rp
        contentTypeBS <- case contentTypeHeaders of
          [x] -> return x
          [] -> putStrLn "Missing Content-Type header, assuming text/html;charset=utf8" >>
                return "text/html;charset=utf8"
          (x:_) -> printf "Multiple Content-Type headers, using %s\n" (decodeUtf8 x) >>
                   return x
        let body = HTTP.getResponseBody rp
        if "text/html" `BS.isPrefixOf` contentTypeBS then do
          let document = HTML.parseLBS body
          metaRefresh <-
            mapM (combineURLs url) .
            mapMaybe (parseMetaRefresh . mconcat . XML.attribute "content") .
            jqQuery ("meta[http-equiv=Refresh]" :: Text) .
            XML.fromDocument $
            document
          case metaRefresh of
            url':_ -> do
              printf "Redirecting due to meta refresh: %s\n" url'
              if url' == url then
                throwHTTPError $ "Infinite redirection (meta refresh): " <> show url
              else
                go url' (n - 1)
            [] -> do
              return (url, body)
        else
          return (url, body)


      where
        redirectCodes :: [Int]
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

combineURLs :: Text -> Text -> IO Text
combineURLs current new = do
  currentURL <- either throwHTTPError return $ parseURLText current
  newURL <- either throwHTTPError return $ parseURLText new
  return $ renderURLText (currentURL <> newURL)
