{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module Main
where

import System.IO
import System.Environment (getArgs)

import FGEFB.Server

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> runServer
    _ -> hPutStrLn stderr "Invalid arguments"
