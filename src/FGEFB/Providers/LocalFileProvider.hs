{-#LANGUAGE LambdaCase #-}
module FGEFB.Providers.LocalFileProvider
where

import Data.Text (Text)
import qualified Data.Text as Text
import System.FilePath ( (</>), makeRelative, takeExtension, takeBaseName )
import System.Directory (listDirectory, doesDirectoryExist)
import System.Process (CreateProcess)
import qualified System.Process as Process
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import Data.Maybe (catMaybes, fromMaybe)
import Control.Applicative ( (<|>) )

import FGEFB.Provider
import FGEFB.LoadPDF

classifyFile :: Text -> FilePath -> FilePath -> FilePath -> IO (Maybe FileInfo)
classifyFile providerID rootdir dirname f = do
  let fullname = rootdir </> dirname </> f
      qname = Text.unpack providerID </> dirname </> f
  doesDirectoryExist fullname >>= \case
    True ->
      return $
        Just FileInfo
          { filePath = qname
          , fileName = Text.pack f
          , fileType = Directory
          }
    False ->
      case takeExtension f of
        ".pdf" ->
          return $
            Just FileInfo
              { filePath = qname
              , fileName = Text.pack $ takeBaseName f
              , fileType = PDFFile
              }
        _ -> return Nothing

localFileProvider :: Maybe Text -> FilePath -> Provider
localFileProvider mlabel rootDir =
  Provider
    { label = mlabel <|> Just (Text.pack $ takeBaseName rootDir)
    , listFiles = \providerID dirname -> do
        listDirectory(rootDir </> dirname) >>=
          (fmap catMaybes . mapM (classifyFile providerID rootDir dirname))
    , getPdfPage = \filename page -> do
        loadPdfPage (rootDir </> filename) page
    }


