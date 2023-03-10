{-# LANGUAGE OverloadedStrings #-}

module FGEFB.Lua.Util
where

import HsLua as Lua
import Control.Exception

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

liftAndRethrowIO :: LuaError e => IO a -> LuaE e a
liftAndRethrowIO action = do
  result <- liftIO (fmap Right action `catch` handler)
  either failLua return result
  where
    handler :: SomeException -> IO (Either String a)
    handler e = return $ Left (displayException e)
