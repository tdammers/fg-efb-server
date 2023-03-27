{-# LANGUAGE OverloadedStrings #-}
module FGEFB.LoadPDF
where

import qualified Data.Text as Text
import Data.Text (Text)
import qualified System.Process as Process
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import System.Directory (doesFileExist)
import Text.Printf (printf)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import Data.Bool (bool)
import Data.Maybe (fromMaybe)
import Control.Monad (void)

import FGEFB.HTTP

-- url -> pdfFilename

loadPdfPage :: FilePath -> Int -> IO (Maybe LBS.ByteString)
loadPdfPage pdfFilename page = do
  jpgFilename <- withCache (Text.pack $ pdfFilename <> "$" <> show page) ".jpg" $ \_ jpgFilename -> do
    void $ convertPdfPage pdfFilename page jpgFilename
  doesFileExist jpgFilename >>= bool
    (return Nothing)
    (Just . LBS.fromStrict <$> BS.readFile jpgFilename)

loadPdfPageHttp :: Text -> Int -> IO (Maybe LBS.ByteString)
loadPdfPageHttp url page = do
  let jpgKey = url <> "$" <> (Text.pack . show $ page)
  jpgFilename <- withCache jpgKey ".jpg" $ \_ jpgFilename -> do
    pdfFilename <- downloadHttp url ".pdf"
    void $ convertPdfPage pdfFilename page jpgFilename
  doesFileExist jpgFilename >>= bool
    (return Nothing)
    (Just . LBS.fromStrict <$> BS.readFile jpgFilename)

convertPdfPage :: FilePath -> Int -> FilePath -> IO Bool
convertPdfPage path page output = do
  magick <- fromMaybe "magick" <$> lookupEnv "MAGICK_BINARY"
  printf "CONVERT %s\n" path
  let args = [ "-limit", "area", "512MiB"
             , "-limit", "disk", "2GiB"
             , "-background", "white"
             , "-density"
             , "300"
             , path ++ "[" ++ show page ++ "]"
             , "-colorspace", "RGB"
             , "-depth", "24"
             , "-quality", "90"
             , "-thumbnail", "4096x4096"
             , "-gravity", "center"
             , "-extent", "4096x4096"
             , output
             ]
  putStrLn $ Process.showCommandForUser magick args
  (exitCode, _out, err) <-
      Process.readProcessWithExitCode magick args ""
  case exitCode of
    ExitSuccess -> do
      return True
    ExitFailure 1 -> do
      putStrLn err
      return False
    ExitFailure e -> do
      putStrLn $ "PDF conversion exited with error " <> show e
      putStrLn err
      error "Something bad happened."

getPdfInfo :: FilePath -> IO [(Text, Text)]
getPdfInfo path = do
  pdfinfo <- fromMaybe "pdfinfo" <$> lookupEnv "PDFINFO_BINARY"
  (exitCode, outStr, errStr) <- Process.readProcessWithExitCode pdfinfo [ path ] ""
  case exitCode of
    ExitSuccess -> do
      return $ map splitPair . Text.lines $ Text.pack outStr
    ExitFailure e -> do
      putStrLn $ "PDF info could not be gathered, error " <> show e
      putStrLn outStr
      putStrLn errStr
      return []
  where
    splitPair :: Text -> (Text, Text)
    splitPair ln =
      let (leftRaw, rightRaw) = Text.breakOn ":" ln
      in (Text.strip leftRaw, Text.strip . Text.drop 1 $ rightRaw)
