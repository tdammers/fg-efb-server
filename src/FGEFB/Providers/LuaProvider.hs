{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module FGEFB.Providers.LuaProvider
where

import Control.Exception
import Control.Monad.Except
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Network.HTTP.Types (urlDecode)
import qualified Text.HTML.DOM as HTML
import Text.Megaparsec.Pos (SourcePos (..), initialPos, unPos)
import qualified Text.XML as XML
import qualified Text.XML.Cursor as XML

import Debug.Trace
import qualified HsLua as Lua

import FGEFB.HTTP (downloadHttp)
import FGEFB.Provider
import FGEFB.URL (renderURLText, parseURLText, URL (..))
import FGEFB.XmlUtil (jqQuery)
import FGEFB.Lua.Util
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
        FileInfo
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
    _context
    mlabel
    scriptFilename =
  Provider
    { label = mlabel
    , getPdf = \pathEnc -> do
        let pathText = unpackParam pathEnc
        runLua "getPDF" pathText (Lua.choice [fmap Just . Lua.peekString, fmap (const Nothing) . Lua.peekNil])
    , listFiles = \pathEnc -> do
        let pathText = unpackParam pathEnc
        runLua "listFiles" pathText (Lua.peekList peekFileInfo)

    }
  where
    httpGET :: Text -> FilePath -> Lua.Lua FilePath
    httpGET url extension =
      Lua.liftIO $ downloadHttp url extension

    runLua :: Lua.Name -> Text -> Lua.Peeker Lua.Exception a -> IO a
    runLua funcname pathText peeker = do
      luaOutput <- Lua.runEither $ do
        Lua.openlibs
        Lua.preloadModule moduleXML
        Lua.preloadModule moduleHTTP
        Lua.registerHaskellFunction "httpGET" httpGET
        luaThrowStatus $ Lua.dofileTrace scriptFilename
        Lua.getglobal funcname
        Lua.push pathText
        luaThrowStatus $ Lua.pcallTrace 1 1
        Lua.force =<< Lua.runPeeker peeker Lua.top
      either throwIO return luaOutput

    unpackParam :: Text -> Text
    unpackParam = decodeUtf8 . urlDecode True . encodeUtf8
