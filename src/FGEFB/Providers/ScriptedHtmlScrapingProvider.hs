{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module FGEFB.Providers.ScriptedHtmlScrapingProvider
where

import Control.Monad (when, forM)
import Control.Monad.Except
import Control.Monad.State
import Data.Aeson ( (.:), (.:?), (.!=) )
import qualified Data.Aeson as JSON
import qualified Data.Aeson.TH as JSON
import qualified Data.ByteString.Lazy as LBS
import Data.List (foldl', sortOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, listToMaybe, maybeToList, fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.Lazy as LText
import Data.Time
import qualified Data.Vector as Vector
import Debug.Trace (trace, traceShow, traceM, traceShowM)
import Network.HTTP.Base (urlEncode, urlDecode)
import qualified Network.HTTP.Conduit as HTTP
import Network.HTTP.Simple (httpJSON, httpBS)
import qualified Network.HTTP.Simple as HTTP
import qualified Network.HTTP.Types as HTTP
import System.FilePath (takeBaseName, dropExtension, (</>), (<.>))
import Text.Casing as Casing
import qualified Text.HTML.DOM as HTML
import Text.Printf (printf)
import Text.Read (readMaybe)
import qualified Text.XML as XML
import qualified Text.XML.Cursor as XML
import qualified Text.XML.Selectors as XML
import qualified Text.XML.Selectors.Parsers.JQ as XML

import FGEFB.LoadPDF
import FGEFB.Provider
import FGEFB.Providers.ScriptedHtmlScrapingProvider.AST
import FGEFB.Providers.ScriptedHtmlScrapingProvider.Interpreter
import FGEFB.Regex
import FGEFB.URL (renderURL, parseURL, URL (..), normalizeURL)
import FGEFB.Util


htmlScrapingProvider :: ProviderContext
                     -> Maybe Text
                     -> Text -- ^ root URL
                     -> Statement -- ^ folder list script
                     -> Statement -- ^ document list script
                     -> Statement -- ^ document URL script
                     -> Provider
htmlScrapingProvider context mlabel rootURLText subdirScript docsScript docURLScript =
  Provider
    { label = mlabel
    , getPdfPage = \pathEnc page -> do
        let pathText = unpackParam pathEnc
            pathURL = either (const NullV) UrlV $ parseURL . unpackParam $ pathEnc
            extraVars = [("pathURL", pathURL), ("pathStr", StringV pathText)] 
        localURL <- runScriptWith
                      asURL
                      extraVars
                      docURLScript
        loadPdfPageHttp (renderURL $ rootURL <> localURL) page
    , listFiles = \pathEnc -> do
        let pathText = unpackParam pathEnc
            pathURL = either (const NullV) UrlV $ parseURL . unpackParam $ pathEnc
            extraVars = [("pathURL", pathURL), ("pathStr", StringV pathText)] 
        subfolders <- runScriptWith (asList >=> mapM (makeLink Directory)) extraVars subdirScript
        docs <- runScriptWith (asList >=> mapM (makeLink PDFFile)) extraVars docsScript
        undefined
    }
  where
    unpackParam :: Text -> Text
    unpackParam = Text.pack . urlDecode . Text.unpack
    
    rootURL :: URL
    rootURL = either error id $ parseURL rootURLText

    defScriptVars :: Map Text Val
    defScriptVars =
      Map.fromList [ ("rootURL", UrlV rootURL) ] <>
      fmap valFromJSON (contextDefs context)

    runScriptWith :: (Val -> Interpret a) -> [(Text, Val)] -> Statement -> IO a
    runScriptWith convert extraDefs stmt = do
      let vars = Map.fromList extraDefs <> defScriptVars
      result <- runInterpret vars (interpretS stmt >>= convert)
      case result of
        Left err -> error err
        Right x -> return x

    makeLink :: FileType -> Val -> Interpret FileInfo
    makeLink fileType val = do
      fileName <- asString =<< lookupMember "name" val
      filePath <- asString =<< lookupMember "path" val
      return FileInfo
        { fileName
        , filePath
        , fileType
        }
