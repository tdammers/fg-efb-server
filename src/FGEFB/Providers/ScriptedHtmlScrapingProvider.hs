{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}

module FGEFB.Providers.ScriptedHtmlScrapingProvider
where

import Control.Exception
import Control.Monad.Except
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Network.HTTP.Types (urlDecode)
import Text.Megaparsec.Pos (SourcePos (..), initialPos, unPos)

import Language.ScrapeScript.AST
import Language.ScrapeScript.Interpreter

import FGEFB.LoadPDF
import FGEFB.Provider
import FGEFB.URL (renderURLText, parseURLText, URL (..))

scriptedHtmlScrapingProvider :: ProviderContext
                     -> Maybe Text
                     -> Text -- ^ root URL
                     -> FilePath -- ^ folder script filename
                     -> Text -- ^ folder script source
                     -> Expr SourcePos -- ^ folder script
                     -> FilePath -- ^ document script filename
                     -> Text -- ^ document script source
                     -> Expr SourcePos -- ^ document script
                     -> Provider
scriptedHtmlScrapingProvider
    context
    mlabel
    rootURLText
    folderScriptFilename folderScriptSrc folderScript
    docScriptFilename docScriptSrc docScript =
  Provider
    { label = mlabel
    , getPdfPage = \pathEnc page -> do
        let pathText = unpackParam pathEnc
            pathURL = either (const NullV) UrlV $ parseURLText . unpackParam $ pathEnc
            extraVars = [("pathURL", pathURL), ("pathStr", StringV pathText)] 
        localURL <- runScriptWith
                      (initialPos docScriptFilename)
                      asURL
                      extraVars
                      docScriptSrc
                      docScript
        loadPdfPageHttp (renderURLText $ rootURL <> localURL) page
    , listFiles = \pathEnc -> do
        let pathText = unpackParam pathEnc
            pathURL = either (const NullV) UrlV $ parseURLText . unpackParam $ pathEnc
            extraVars = [("pathURL", pathURL), ("pathStr", StringV pathText)] 
        runScriptWith
          (initialPos folderScriptFilename)
          (asList >=> mapM makeLink)
          extraVars
          folderScriptSrc
          folderScript
    }
  where
    unpackParam :: Text -> Text
    unpackParam = decodeUtf8 . urlDecode True . encodeUtf8
    
    rootURL :: URL
    rootURL = either error id $ parseURLText rootURLText

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
