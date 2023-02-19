{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module FGEFB.Providers.ScriptedHtmlScrapingProvider
where

import Control.Monad.Except
import Control.Exception
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Network.HTTP.Base (urlDecode)
import Text.Megaparsec.Pos (SourcePos, initialPos)

import Language.ScrapeScript.AST
import Language.ScrapeScript.Interpreter

import FGEFB.LoadPDF
import FGEFB.Provider
import FGEFB.URL (renderURL, parseURL, URL (..))

scriptedHtmlScrapingProvider :: ProviderContext
                     -> Maybe Text
                     -> Text -- ^ root URL
                     -> Expr SourcePos -- ^ folder list script
                     -> Expr SourcePos -- ^ document list script
                     -> Expr SourcePos -- ^ document URL script
                     -> Provider
scriptedHtmlScrapingProvider context mlabel rootURLText subdirScript docsScript docURLScript =
  Provider
    { label = mlabel
    , getPdfPage = \pathEnc page -> do
        let pathText = unpackParam pathEnc
            pathURL = either (const NullV) UrlV $ parseURL . unpackParam $ pathEnc
            extraVars = [("pathURL", pathURL), ("pathStr", StringV pathText)] 
        localURL <- runScriptWith
                      (initialPos "document script")
                      asURL
                      extraVars
                      docURLScript
        loadPdfPageHttp (renderURL $ rootURL <> localURL) page
    , listFiles = \pathEnc -> do
        let pathText = unpackParam pathEnc
            pathURL = either (const NullV) UrlV $ parseURL . unpackParam $ pathEnc
            extraVars = [("pathURL", pathURL), ("pathStr", StringV pathText)] 
        subfolders <-
          runScriptWith
            (initialPos "folders script")
            (asList >=> mapM (makeLink Directory))
            extraVars
            subdirScript
        docs <-
          runScriptWith
            (initialPos "documents script")
            (asList >=> mapM (makeLink PDFFile))
            extraVars
            docsScript
        return $ subfolders ++ docs
    }
  where
    unpackParam :: Text -> Text
    unpackParam = Text.pack . urlDecode . Text.unpack
    
    rootURL :: URL
    rootURL = either error id $ parseURL rootURLText

    defScriptVars :: Map Text (Val SourcePos)
    defScriptVars =
      Map.fromList [ ("rootURL", UrlV rootURL) ] <>
      fmap valFromJSON (contextDefs context)

    runScriptWith :: SourcePos
                  -> (Val SourcePos -> Interpret SourcePos a)
                  -> [(Text, Val SourcePos)]
                  -> Expr SourcePos
                  -> IO a
    runScriptWith p convert extraDefs stmt = do
      let vars = Map.fromList extraDefs <> defScriptVars
      result <- runInterpret p vars (eval stmt >>= convert)
      case result of
        Left err -> throw err
        Right x -> return x

    makeLink :: FileType -> Val SourcePos -> Interpret SourcePos FileInfo
    makeLink fileType val = do
      fileName <- asUrlString =<< lookupMember "name" val
      filePath <- asUrlString =<< lookupMember "path" val
      return FileInfo
        { fileName
        , filePath
        , fileType
        }
