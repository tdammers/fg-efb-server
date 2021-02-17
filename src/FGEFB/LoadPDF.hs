{-#LANGUAGE TypeApplications #-}
{-#LANGUAGE LambdaCase #-}
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

downloadPdfPageHttp :: Text -> IO FilePath
downloadPdfPageHttp url = do
  printf "Load PDF: %s\n" url
  rq <- HTTP.parseRequest (Text.unpack url)
  tmpdir <- do
    lookupEnv "PDFCACHE" >>= \case
      Nothing ->
        getCanonicalTemporaryDirectory
      Just dir ->
        return dir
  printf "Cache dir: %s\n" tmpdir
  let basename = show (Crypto.hash (encodeUtf8 url) :: Crypto.Digest SHA512)
      filename = tmpdir </> ("fgefb-cache-" ++  basename) <.> "pdf"
  putStrLn filename
  doesFileExist filename >>= \case
    False -> do
      printf "HTTP %s -> %s\n" url filename
      rp <- HTTP.httpBS rq
      BS.writeFile filename (HTTP.getResponseBody rp)
    _ -> do
      printf "FILE %s\n" filename
      return ()
  return filename

loadPdfPageHttp :: Text -> Int -> IO LBS.ByteString
loadPdfPageHttp url page = do
  printf "Requested PDF: %s [%i]\n" url page
  filename <- downloadPdfPageHttp url
  putStrLn filename
  loadPdfPage filename page

loadPdfPage :: FilePath -> Int -> IO LBS.ByteString
loadPdfPage path page = do
  let cp = (Process.proc "convert"
            [ "-background", "white"
            , "-density"
            , "150"
            , path ++ "[" ++ show page ++ "]"
            , "-alpha", "off"
            , "-colorspace", "RGB"
            , "-depth", "24"
            , "-quality", "99"
            , "jpg:-"
            ]
          )
          { Process.std_out = Process.CreatePipe
          , Process.std_err = Process.Inherit
          }
  Process.withCreateProcess cp $ \_ mout _ ph -> do
      case mout of
        Nothing ->
          error "Something bad happened."
        Just out -> do
          result <- LBS.fromStrict <$> BS.hGetContents out
          Process.waitForProcess ph >>= \case
            ExitSuccess ->
              return result
            ExitFailure e ->
              error "Something bad happened."
