{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module FGEFB.Providers.ScriptedHtmlScrapingProvider
where

import Control.Monad.Except
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Network.HTTP.Base (urlDecode)

import Language.ScrapeScript.AST
import Language.ScrapeScript.Interpreter

import FGEFB.LoadPDF
import FGEFB.Provider
import FGEFB.URL (renderURL, parseURL, URL (..))

scriptedHtmlScrapingProvider :: ProviderContext
                     -> Maybe Text
                     -> Text -- ^ root URL
                     -> Expr -- ^ folder list script
                     -> Expr -- ^ document list script
                     -> Expr -- ^ document URL script
                     -> Provider
scriptedHtmlScrapingProvider context mlabel rootURLText subdirScript docsScript docURLScript =
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
        return $ subfolders ++ docs
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

    runScriptWith :: (Val -> Interpret a) -> [(Text, Val)] -> Expr -> IO a
    runScriptWith convert extraDefs stmt = do
      let vars = Map.fromList extraDefs <> defScriptVars
      result <- runInterpret vars (eval stmt >>= convert)
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
