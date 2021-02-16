{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE OverloadedLists #-}
{-#LANGUAGE LambdaCase #-}
module Main
where

import qualified Web.Scotty as Scotty
import Web.Scotty (ScottyM, scotty)
import Control.Monad (forM_)
import qualified Text.XML as XML
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import Data.Default (def)
import qualified System.Process as Process
import System.Process (CreateProcess)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import System.FilePath ( (</>), makeRelative, takeExtension, takeBaseName )
import System.Directory (listDirectory, doesDirectoryExist)
import System.Environment (getArgs)
import Data.Maybe (catMaybes)

app :: FilePath -> ScottyM ()
app pdfDir = do
  -- redirect if missing trailing slash
  Scotty.get (Scotty.regex "^/(([a-zA-Z0-9_ \\-]+/)*[a-zA-Z0-9_ \\-]+)$") $ do
    dirname <- Scotty.param "0"
    Scotty.redirect $ dirname <> "/"
  -- directory listings
  Scotty.get (Scotty.regex "^/(([a-zA-Z0-9_ \\-]+/)*)$") $ do
    dirname <- Scotty.param "1"
    -- Scotty.liftAndCatchIO $ putStrLn dirname
    files <- Scotty.liftAndCatchIO $
              listDirectory(pdfDir </> dirname) >>=
              (fmap catMaybes . mapM (classifyFile pdfDir dirname))
    Scotty.setHeader "Content-type" "text/xml"
    let filesXml = xmlFileList files
    Scotty.raw . XML.renderLBS def . xmlFragmentToDocument $ filesXml

  -- PDF pages
  Scotty.get (Scotty.regex "^/(([a-zA-Z0-9_ \\-]+/)*[a-zA-Z0-9_ \\-]+\\.pdf)$") $ do
    filename <- fmap (pdfDir </>) $ Scotty.param "1"
    -- Scotty.liftAndCatchIO $ putStrLn filename
    page <- Scotty.param "p" `Scotty.rescue` const (return 0)
    body <- Scotty.liftAndCatchIO $ getPdfPage filename page
    Scotty.setHeader "Content-type" "image/jpeg"
    Scotty.raw body

  Scotty.get (Scotty.regex "^.*$") $ do
    -- path <- Scotty.param "0"
    -- Scotty.liftAndCatchIO $ putStrLn path
    Scotty.next

getPdfPage :: FilePath -> Int -> IO LBS.ByteString
getPdfPage pdfFilePath page = do
  let cp = (Process.proc "convert"
            [ "-background", "white"
            , "-density"
            , "150"
            , pdfFilePath ++ "[" ++ show page ++ "]"
            , "-quality", "99"
            , "jpeg:-"
            ]
          )
          { Process.std_out = Process.CreatePipe
          }
  Process.withCreateProcess cp $ \_ mout _ _ -> do
    case mout of
      Nothing ->
        error "Something bad happened."
      Just out ->
        LBS.fromStrict <$> BS.hGetContents out

xmlFragmentToDocument :: XML.Element -> XML.Document
xmlFragmentToDocument docroot =
  XML.Document
    { XML.documentPrologue = XML.Prologue [] Nothing []
    , XML.documentEpilogue = []
    , XML.documentRoot = docroot
    }

data FileType
  = Directory
  | PDFFile

classifyFile :: FilePath -> FilePath -> FilePath -> IO (Maybe (FileType, FilePath))
classifyFile rootdir dirname f = do
  let fullname = rootdir </> dirname </> f
      qname = dirname </> f
  doesDirectoryExist fullname >>= \case
    True ->
      return (Just (Directory, f))
    False ->
      case takeExtension f of
        ".pdf" -> return (Just (PDFFile, qname))
        _ -> return Nothing

xmlFileList :: [(FileType, FilePath)] -> XML.Element
xmlFileList files =
  XML.Element "listing" [] (map (uncurry xmlFileEntry) files)

xmlFileEntry :: FileType -> FilePath -> XML.Node
xmlFileEntry Directory filePath =
    XML.NodeElement $
      XML.Element "directory"
        []
        [ XML.NodeElement $ XML.Element "name" [] [ XML.NodeContent (Text.pack $ takeBaseName filePath) ]
        , XML.NodeElement $ XML.Element "path" [] [ XML.NodeContent (Text.pack filePath) ]
        ]
xmlFileEntry PDFFile filePath =
    XML.NodeElement $
      XML.Element "file"
        []
        [ XML.NodeElement $ XML.Element "name" [] [ XML.NodeContent (Text.pack $ takeBaseName filePath) ]
        , XML.NodeElement $ XML.Element "path" [] [ XML.NodeContent (Text.pack filePath) ]
        , XML.NodeElement $ XML.Element "type" [] [ XML.NodeContent ("pfd") ]
        ]

main = do
  [pdfDir] <- getArgs
  scotty 7675 (app pdfDir)
