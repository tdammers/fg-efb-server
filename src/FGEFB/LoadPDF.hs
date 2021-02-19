{-#LANGUAGE TypeApplications #-}
{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE OverloadedStrings #-}
module FGEFB.LoadPDF
where

import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Crypto.Hash as Crypto
import Crypto.Hash.Algorithms (SHA512)
import System.IO.Temp (getCanonicalTemporaryDirectory)
import qualified Network.HTTP.Simple as HTTP
import qualified System.Process as Process
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import System.FilePath ( (</>), (<.>) )
import System.Directory (doesFileExist)
import Text.Printf (printf)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import Data.Bool (bool)

getCacheDir :: IO FilePath
getCacheDir = do
  lookupEnv "PDFCACHE" >>= \case
    Nothing -> getCanonicalTemporaryDirectory
    Just dir -> return dir

getCacheFilename :: Text -> FilePath -> IO FilePath
getCacheFilename url extension = do
  tmpdir <- getCacheDir
  let basename = show (Crypto.hash (encodeUtf8 url) :: Crypto.Digest SHA512)
  return $ tmpdir </> ("fgefb-cache-" ++  basename) <.> extension

downloadPdfPageHttp :: Text -> FilePath -> IO ()
downloadPdfPageHttp url filename = do
  printf "Load PDF: %s\n" url
  doesFileExist filename >>= \case
    False -> do
      rq <- HTTP.parseRequest (Text.unpack url)
      printf "HTTP %s -> %s\n" url filename
      rp <- HTTP.httpBS rq
      BS.writeFile filename (HTTP.getResponseBody rp)
    _ -> do
      printf "FILE %s\n" filename
      return ()

loadPdfPageHttp :: Text -> Int -> IO (Maybe LBS.ByteString)
loadPdfPageHttp url page = do
  printf "Requested PDF: %s [%i]\n" url page
  jpgFilename <- getCacheFilename (url <> "$" <> (Text.pack . show $ page)) ".jpg"
  doesFileExist jpgFilename >>= \case
    True -> do
      printf "JPG FILE %s FOUND\n" jpgFilename
      Just . LBS.fromStrict <$> BS.readFile jpgFilename

    False -> do
      printf "JPG FILE %s NOT FOUND\n" jpgFilename
      pdfFilename <- getCacheFilename url ".pdf"
      downloadPdfPageHttp url pdfFilename
      convertPdfPage pdfFilename page jpgFilename >>= bool
          (return Nothing)
          (Just . LBS.fromStrict <$> BS.readFile jpgFilename)

loadPdfPage :: FilePath -> Int -> IO (Maybe LBS.ByteString)
loadPdfPage path page = do
  jpgFilename <- getCacheFilename (Text.pack $ path <> show page) ".jpg"
  doesFileExist jpgFilename >>= \case
    True -> do
      printf "JPG FILE %s FOUND\n" jpgFilename
      Just . LBS.fromStrict <$> BS.readFile jpgFilename
    False -> do
      printf "JPG FILE %s NOT FOUND\n" jpgFilename
      convertPdfPage path page jpgFilename >>= bool
          (return Nothing)
          (Just . LBS.fromStrict <$> BS.readFile jpgFilename)

convertPdfPage :: FilePath -> Int -> FilePath -> IO Bool
convertPdfPage path page output = do
  printf "CONVERT %s\n" path
  let cp = (Process.proc "convert"
            [ "-background", "white"
            , "-density"
            , "300"
            , path ++ "[" ++ show page ++ "]"
            , "-alpha", "off"
            , "-colorspace", "RGB"
            , "-depth", "24"
            , "-quality", "99"
            , "jpg:" ++ output
            ]
          )
          { Process.std_out = Process.Inherit
          , Process.std_err = Process.Inherit
          }
  Process.withCreateProcess cp $ \_ _ _ ph -> do
    Process.waitForProcess ph >>= \case
      ExitSuccess ->
        return True
      ExitFailure 1 ->
        return False
      ExitFailure e ->
        error "Something bad happened."
