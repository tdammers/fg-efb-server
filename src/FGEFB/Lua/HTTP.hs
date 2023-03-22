{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}

module FGEFB.Lua.HTTP
where

import HsLua as Lua
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString (ByteString)
import Network.HTTP.Types (urlEncode)

import FGEFB.Lua.Util
import FGEFB.HTTP
import FGEFB.URL

flipPair :: (a, b) -> (b, a)
flipPair (a, b) = (b, a)

moduleHTTP :: LuaError e => Module e
moduleHTTP =
  Module
    "http"
    "HTTP"
    -- fields
    []
    -- functions
    [ defun "download"
        ### (\url extension -> liftAndRethrowIO (downloadHttp url extension))
        <#> parameter peekText "string" "url" "URL to fetch from"
        <#> parameter peekString "string" "extension" "file extension for cache file"
        =#> functionResult pushString "string" "filename"
    , defun "get"
        ### (\url -> do
              (finalURL, body) <- liftAndRethrowIO (httpCachedGET url)
              pushByteString (LBS.toStrict body)
              pushText finalURL
              return 2
            )
        <#> parameter peekText "string" "url" "URL to fetch from"
        =?> "body, url"
    ]
    -- operations
    []

joinURLs :: LuaError e => ByteString -> ByteString -> LuaE e ByteString
joinURLs a b = either fail return $ do
  left <- parseURL a
  right <- parseURL b
  return . renderURL $ left <> right

moduleURL :: LuaError e => Module e
moduleURL =
  Module
    "url"
    "URLs"
    -- fields
    []
    -- functions
    [ defun "parse"
        ### (either fail return . parseURLText)
        <#> parameter peekText "string" "url" "URL in string form"
        =#> functionResult push "object" "parsed URL"
    , defun "join"
        ### joinURLs
        <#> parameter peekByteString "string" "left" "left URL"
        <#> parameter peekByteString "string" "right" "right URL"
        =#> functionResult push "string" "combined URL"
    , defun "encode"
        ### liftPure (urlEncode True)
        <#> parameter peekByteString "string" "input" "raw string"
        =#> functionResult push "string" "url-encoded string"
    ]
    -- operations
    []

pushProtocol :: Protocol -> LuaE e ()
pushProtocol HTTP = pushText "http"
pushProtocol HTTPS = pushText "https"

peekProtocol :: LuaError e => Peeker e Protocol
peekProtocol idx = do
  t <- peekText idx
  case t of
    "http" -> return HTTP
    "https" -> return HTTPS
    _ -> fail $ "Invalid protocol " ++ show t

typeURL :: LuaError e => DocumentedType e URL
typeURL =
  deftype "URL"
    -- operations
    [ operation Tostring $ defun "__tostring"
        ### liftPure renderURL
        <#> udparam typeURL "url" "object"
        =#> functionResult pushByteString "string" "stringified name"
    , operation Concat $ defun "__concat"
        ### liftPure2 (<>)
        <#> udparam typeURL "left" "object"
        <#> udparam typeURL "right" "object"
        =#> functionResult push "object" "combined URL"
    ]
    -- members
    [ property
        "protocol"
        "protocol (http or https)"
        (pushMaybe pushProtocol, urlProtocol)
        (peekMaybe peekProtocol, \u x -> u { urlProtocol = x })
    , property
        "host"
        "host name"
        (pushMaybe pushByteString, urlHostName)
        (peekMaybe peekByteString, \u x -> u { urlHostName = x })
    , property
        "path"
        "path (/foo/bar/baz; split up into a list of strings)"
        (pushList pushText, urlPath)
        (peekList peekText, \u x -> u { urlPath = x })
    , property
        "query"
        "query (?key1=value1&key2=value2; split up into a key-value table)"
        (pushMaybe $ pushKeyValuePairs pushText (pushMaybe pushText), urlQuery)
        (peekMaybe $ peekKeyValuePairs peekText (peekMaybe peekText), \u x -> u { urlQuery = x })
    , property
        "anchor"
        "anchor (#target)"
        (pushMaybe pushByteString, urlAnchor)
        (peekMaybe peekByteString, \u x -> u { urlAnchor = x })
    ]

instance Pushable URL where
  push = pushUD typeURL

instance Peekable URL where
  safepeek = peekUD typeURL
