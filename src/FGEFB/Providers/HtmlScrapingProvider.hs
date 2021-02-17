{-#LANGUAGE DeriveFunctor #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE ScopedTypeVariables #-}
module FGEFB.Providers.HtmlScrapingProvider
where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import qualified Data.Aeson as JSON
import qualified Data.Aeson.TH as JSON
import Network.HTTP.Simple (httpJSON, httpBS)
import qualified Network.HTTP.Simple as HTTP
import Text.Read (readMaybe)
import System.FilePath (takeBaseName, dropExtension, (</>), (<.>))
import Control.Monad (when, forM)
import qualified Text.HTML.DOM as HTML
import qualified Text.XML.Cursor as HTML
import qualified Text.XML.Scraping as HTML
import qualified Text.XML.Selector as HTML
import qualified Text.XML as XML
import Data.Maybe (catMaybes)
import Network.HTTP.Base (urlEncode, urlDecode)

import FGEFB.Provider
import FGEFB.LoadPDF

htmlScrapingProvider :: Maybe Text
                     -> Text -- ^ root URL
                     -> Text -- ^ landing path
                     -> Text -- ^ selector for folder links
                     -> Maybe String -- ^ selector for extracting folder URL from folder link
                     -> Maybe Text -- ^ attribute name for extracting folder URL from folder link
                     -> Maybe String -- ^ selector for extracting folder label from folder link
                     -> Maybe Text -- ^ attribute name for extracting folder label from folder link
                     -> Text -- ^ selector for document links
                     -> Maybe String -- ^ selector for extracting document URL from document link
                     -> Maybe Text -- ^ attribute name for extracting document URL from document link
                     -> Maybe String -- ^ selector for extracting document label from document link
                     -> Maybe Text -- ^ attribute name for extracting document label from document link
                     -> Provider
htmlScrapingProvider
  mlabel
  rootUrl landingPath
  folderSel folderUrlSel folderUrlAttrib folderLabelSel folderLabelAttrib
  documentSel documentUrlSel documentUrlAttrib documentLabelSel documentLabelAttrib =
  Provider
    { label = mlabel
    , getPdfPage = \filenameEnc page -> do
        let filename = urlDecode . dropExtension $ filenameEnc
        when (take 1 filename /= "/") (error $ "Invalid path: " <> show filename)
        loadPdfPageHttp (rootUrl <> Text.pack filename) page
    , listFiles = \providerID pathEnc -> do
        let path = urlDecode pathEnc
        putStrLn path
        let actualPath = if null path then Text.unpack landingPath else path
        when (take 1 actualPath /= "/") (error $ "Invalid path: " <> show actualPath)
        rq <- HTTP.parseRequest (Text.unpack rootUrl <> actualPath)
        rawHtml <- HTTP.getResponseBody <$> HTTP.httpLBS rq
        let document = HTML.parseLBS rawHtml
            root = HTML.fromDocument document
            folderInfos = captureLinks
                            providerID
                            Directory
                            folderSel
                            folderUrlSel
                            folderUrlAttrib
                            folderLabelSel
                            folderLabelAttrib
                            root
            documentInfos = captureLinks
                            providerID
                            PDFFile
                            documentSel
                            documentUrlSel
                            documentUrlAttrib
                            documentLabelSel
                            documentLabelAttrib
                            root
        return $ folderInfos ++ documentInfos
    }
    where
      lookupAttrib :: Maybe Text -> HTML.Cursor -> Text
      lookupAttrib Nothing e = LText.toStrict $ HTML.innerText e
      lookupAttrib (Just attrib) e = mconcat $ HTML.attribute (XML.Name attrib Nothing Nothing) e

      captureLinks :: Text
                   -> FileType
                   -> Text
                   -> Maybe String
                   -> Maybe Text
                   -> Maybe String
                   -> Maybe Text
                   -> HTML.Cursor
                   -> [FileInfo]
      captureLinks providerID fty elemSels urlSel urlAttrib labelSel labelAttrib root =
        let elems = concatMap (\elemSel -> HTML.query elemSel root) (map Text.unpack . Text.splitOn "," $ elemSels)
            links = catMaybes $ for elems (\e -> do
                      label <- lookupAttrib labelAttrib <$> maybe Just HTML.query1 labelSel e
                      path <- lookupAttrib urlAttrib <$> maybe Just HTML.query1 urlSel e
                      return (label, path))
            extend = if fty == Directory then id else (<.> "pdf")
        in [ FileInfo
               { fileName = Text.unwords . Text.words $ label
               , filePath =
                  extend $ Text.unpack providerID </> (urlEncode . Text.unpack $ path)
               , fileType = fty
               }
           | (label, path) <- links
           ]

for = flip map
