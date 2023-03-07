{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}

module FGEFB.Lua.HTTP
where

import HsLua as Lua
import qualified Data.ByteString.Lazy as LBS

import FGEFB.Lua.Util
import FGEFB.HTTP

moduleHTTP :: LuaError e => Module e
moduleHTTP =
  Module
    "http"
    "HTTP"
    -- fields
    []
    -- functions
    [ defun "download"
        ### (\url extension -> liftIO (downloadHttp url extension))
        <#> parameter peekText "string" "url" "URL to fetch from"
        <#> parameter peekString "string" "extension" "file extension for cache file"
        =#> functionResult pushString "string" "filename"
    , defun "get"
        ### (\url -> LBS.toStrict . snd <$> liftIO (httpCachedGET url))
        <#> parameter peekText "string" "url" "URL to fetch from"
        =#> functionResult pushByteString "string" "body"
    ]
    -- operations
    []

