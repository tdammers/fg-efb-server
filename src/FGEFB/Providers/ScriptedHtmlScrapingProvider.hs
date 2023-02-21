{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}

module FGEFB.Providers.ScriptedHtmlScrapingProvider
where

import Control.Monad.Except
import Control.Exception
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Network.HTTP.Base (urlDecode)
import Text.Megaparsec.Pos (SourcePos (..), initialPos, unPos)

import Language.ScrapeScript.AST
import Language.ScrapeScript.Interpreter

import FGEFB.LoadPDF
import FGEFB.Provider
import FGEFB.URL (renderURL, parseURL, URL (..))

scriptedHtmlScrapingProvider :: ProviderContext
                     -> Maybe Text
                     -> Text -- ^ root URL
                     -> Text -- ^ folder script source
                     -> Expr SourcePos -- ^ folder script
                     -> Text -- ^ document script source
                     -> Expr SourcePos -- ^ document script
                     -> Provider
scriptedHtmlScrapingProvider
    context
    mlabel
    rootURLText
    folderScriptSrc folderScript
    docScriptSrc docScript =
  Provider
    { label = mlabel
    , getPdfPage = \pathEnc page -> do
        let pathText = unpackParam pathEnc
            pathURL = either (const NullV) UrlV $ parseURL . unpackParam $ pathEnc
            extraVars = [("pathURL", pathURL), ("pathStr", StringV pathText)] 
        localURL <- runScriptWith
                      (initialPos "document")
                      asURL
                      extraVars
                      docScriptSrc
                      docScript
        loadPdfPageHttp (renderURL $ rootURL <> localURL) page
    , listFiles = \pathEnc -> do
        let pathText = unpackParam pathEnc
            pathURL = either (const NullV) UrlV $ parseURL . unpackParam $ pathEnc
            extraVars = [("pathURL", pathURL), ("pathStr", StringV pathText)] 
        links <-
          runScriptWith
            (initialPos "folder")
            (asList >=> mapM makeLink)
            extraVars
            folderScriptSrc
            folderScript
        return links
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
                  -> Text
                  -> Expr SourcePos
                  -> IO a
    runScriptWith p convert extraDefs src stmt = do
      let vars = Map.fromList extraDefs <> defScriptVars
      result <- runInterpret p vars (eval stmt >>= convert)
      case result of
        Left err -> do
          let errorLoc = runtimeErrorLocation err
          let errorLine = Text.unpack $ Text.lines src !! (unPos (sourceLine errorLoc) - 1)
              errorMarker = replicate (unPos (sourceColumn errorLoc) - 1) ' ' ++ "^"
          throw err
            { runtimeErrorMessage =
                unlines [ runtimeErrorMessage err, errorLine, errorMarker ]
            }
        Right x -> return x

    makeLink :: Val SourcePos -> Interpret SourcePos FileInfo
    makeLink val = do
      fileName <- lookupMember "name" val >>= asUrlString
      filePath <- lookupMember "path" val >>= asUrlString
      fileType <- lookupMember "type" val >>= asString >>= \case
        "dir" -> return Directory
        "pdf" -> return PDFFile
        x -> throwRuntimeError $
                "Expected \"dir\" or \"pdf\", but got " ++ show x
      return FileInfo
        { fileName
        , filePath
        , fileType
        }
