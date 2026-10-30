{-# LANGUAGE OverloadedStrings #-}

module FGEFB.Lua.JSON
where

import Control.Monad (forM_)
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.ByteString (ByteString)
import qualified Data.Vector as V
import HsLua as Lua
import HsLua.Aeson (pushValue)

moduleJSON :: LuaError e => Module e
moduleJSON =
  Module
    "json"
    "JSON parsing"
    -- fields
    []
    -- functions
    [ defun "parse"
        ### (\bs -> either Lua.failLua return (JSON.eitherDecodeStrict bs))
        <#> parameter peekByteString "string" "src" "JSON source"
        =#> functionResult pushValue "any" "parsed value"
    ]
    -- operations
    []
    -- type initializers
    []
    -- type docs
    []
