{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module FGEFB.Chartfox
where

import FGEFB.HTTP (httpPOST, httpGETWithHeaders, RequestBody (..))
import qualified Crypto.Hash as Crypto
import qualified Crypto.Random.Entropy as Crypto
import Crypto.Hash.Algorithms (SHA256)
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Char
import Data.Word
import Data.Bits
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteArray as BA
import Data.ByteString.Base64 as Base64
import qualified Data.Aeson as JSON
import Data.Aeson ( (.:) )

import FGEFB.URL

encodeVerifier :: ByteString -> ByteString
encodeVerifier rawEntropy = do
  BS.pack $ map encodeVerifierChar bytes
  where
    bytes :: [Word8]
    bytes = BS.unpack rawEntropy

    alphabet :: [Word8]
    alphabet = map (fromIntegral . ord) $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "-_"

    encodeVerifierChar c =
      alphabet !! (fromIntegral c .&. 63)

encodeBase64URL :: ByteString -> ByteString
encodeBase64URL =
  BS.dropWhileEnd (== fromIntegral (ord '=')) . BS.map fixByte .  Base64.encode
  where
    fixByte :: Word8 -> Word8
    fixByte b
      | b == fromIntegral (ord '+')
      = fromIntegral $ ord '-'
      | b == fromIntegral (ord '/')
      = fromIntegral $ ord '_'
      | otherwise
      = b

generateChallenge :: IO (ByteString, ByteString)
generateChallenge = do
  verifier <- encodeVerifier <$> Crypto.getEntropy 64
  let challengeRaw = Crypto.hash verifier :: Crypto.Digest SHA256
  let challenge = BA.convert challengeRaw
  print verifier
  print $ challengeRaw
  print $ encodeBase64URL challenge
  return (encodeBase64URL challenge, verifier)

data BearerTokens =
  BearerTokens
    { expiry :: !Int
    , accessToken :: !Text
    , refreshToken :: !Text
    }
    deriving (Show)

instance JSON.FromJSON BearerTokens where
  parseJSON = JSON.withObject "BearerTokens" $ \o ->
    BearerTokens
      <$> o .: "expires_in"
      <*> o .: "access_token"
      <*> o .: "refresh_token"

getToken :: ByteString -> ByteString -> ByteString -> IO (Maybe BearerTokens)
getToken clientID code verifier = do
  let formData =
        [ ("grant_type", "authorization_code")
        , ("client_id", clientID)
        , ("code", code)
        , ("code_verifier", verifier)
        , ("redirect_uri", "http://localhost:10000/aircraft-dir/WebPanel/chartfox_oauth.html")
        ]
  (_finalUrl, content) <-
    httpPOST "https://api.chartfox.org/oauth/token"
      (WWWFormUrlencodedRequestBody formData)
  let tokensEither = JSON.eitherDecode content
  case tokensEither of
    Right tokens -> do
      return (Just tokens)
    Left err -> do
      print err
      return Nothing

apiCall :: JSON.FromJSON a => Text -> Text -> [(Text, Maybe Text)] -> IO (Either String a)
apiCall token path query = do
  let urlEither =
        fmap (appendQuery (("token", Just token) : ("per_page", Just "12") : query)) .
        parseURLText $ "https://api.chartfox.org/" <> path
  case urlEither of
    Left err ->
      return (Left err)
    Right url -> do
      (_finalUrl, content) <-
        httpGETWithHeaders
          (renderURLText url)
          [ ("Accept", "application/json")
          , ("Authorization", "Bearer " <> encodeUtf8 token)
          ]
      return $ JSON.eitherDecode content

