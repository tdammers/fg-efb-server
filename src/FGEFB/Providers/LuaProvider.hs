{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module FGEFB.Providers.LuaProvider
where

import Control.Exception
import Control.Monad
-- import Control.Monad.Except
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Network.HTTP.Types (urlDecode)

import qualified HsLua as Lua

import FGEFB.Provider
import FGEFB.Lua.XML
import FGEFB.Lua.HTTP

luaThrowStatus :: Lua.LuaError e => Lua.LuaE e Lua.Status -> Lua.LuaE e ()
luaThrowStatus action = do
  result <- action
  when (result /= Lua.OK) $ do
    msg <- Lua.peek 1
    Lua.failLua $ show result ++ ": " ++ msg

peekFileInfo :: Lua.LuaError e => Lua.Peeker e FileInfo
peekFileInfo index = do
  m :: Map Text Text <- Lua.peekMap Lua.safepeek Lua.safepeek index
  let fileInfoMaybe =
        fileInfo
          <$> Map.lookup "name" m
          <*> Map.lookup "path" m
          <*> (Map.lookup "type" m >>= \case
                "pdf" -> return PDFFile
                "dir" -> return Directory
                _ -> Nothing
              )
  maybe (Lua.failPeek "invalid FileInfo") return fileInfoMaybe

luaProvider :: ProviderContext
            -> Maybe Text
            -> FilePath
            -> Provider
luaProvider
    context
    mlabel
    scriptFilename =
  Provider
    { label = mlabel
    , getPdf = \_ pathEnc -> do
        let pathText = unpackParam pathEnc
        fmap simpleFileDetails <$>
                runLua
                  "getPDF" [Lua.push pathText]
                  (Lua.choice [fmap Just . Lua.peekString, fmap (const Nothing) . Lua.peekNil])
    , listFiles = \query pathEnc page -> do
        let pathText = unpackParam pathEnc
        items <- paginate page <$>
                    runLua
                      "listFiles" [Lua.push pathText, Lua.push $ Map.fromList query]
                      (Lua.peekList peekFileInfo)
        case pathText of
          "" -> amendSearchPath items
          "/" -> amendSearchPath items
          _ -> return items

    , available = const True
    }
  where
    amendSearchPath items@FileList { fileListMeta = meta@FileListMeta {..}} = do
      searchPathMaybe <- getLua "searchPath"
                      (Lua.choice
                        [ fmap Just .  Lua.peekText
                        , fmap (const Nothing) . Lua.peekNil
                        ])
      return $ 
          items
            { fileListMeta = meta
              { fileListMetaSearchPath = searchPathMaybe
              }
            }

    execLua :: Lua.LuaE Lua.Exception () -> Lua.Peeker Lua.Exception a -> IO a
    execLua inner peeker = do
      luaOutput <- Lua.runEither $ do
        Lua.openlibs
        void $ Lua.dostring "package.path = './provider-scripts/?.lua'"
        void $ Lua.dostring "package.cpath = ''"
        Lua.preloadModule moduleXML
        Lua.preloadModule moduleHTTP
        Lua.preloadModule moduleURL
        Lua.pushMap Lua.pushText Lua.pushValue (contextDefs context)
        Lua.setglobal "context"
        luaThrowStatus $ Lua.dofileTrace (Just scriptFilename)
        inner
        Lua.force =<< Lua.runPeeker peeker Lua.top
      either throwIO return luaOutput

    getLua :: Lua.Name -> Lua.Peeker Lua.Exception a -> IO a
    getLua varname =
      execLua $ do
        void $ Lua.getglobal varname

    runLua :: Lua.Name -> [Lua.LuaE Lua.Exception ()] -> Lua.Peeker Lua.Exception a -> IO a
    runLua funcname args =
      execLua $ do
        void $ Lua.getglobal funcname
        sequence_ args
        luaThrowStatus $ Lua.pcallTrace (fromIntegral $ length args) 1

    unpackParam :: Text -> Text
    unpackParam = decodeUtf8 . urlDecode True . encodeUtf8
