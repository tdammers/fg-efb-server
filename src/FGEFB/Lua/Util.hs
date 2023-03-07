{-# LANGUAGE OverloadedStrings #-}

module FGEFB.Lua.Util
where

import HsLua as Lua

peekMaybe :: LuaError e => Peeker e a -> Peeker e (Maybe a)
peekMaybe peeker =
  choice
    [ fmap (const Nothing) . peekNil
    , fmap Just . peeker
    ]

pushMaybe :: LuaError e => (a -> LuaE e ()) -> Maybe a -> LuaE e ()
pushMaybe pushJust (Just x) = pushJust x
pushMaybe _ Nothing = pushnil

maybePossible :: Possible a -> Maybe a
maybePossible (Actual x) = Just x
maybePossible Absent = Nothing
