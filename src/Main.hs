{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module Main
where

import System.IO
import System.Environment (getArgs)

import FGEFB.Server
import FGEFB.REPL

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["repl"] -> runREPL
    [] -> runServer
    _ -> hPutStrLn stderr "Invalid arguments"
