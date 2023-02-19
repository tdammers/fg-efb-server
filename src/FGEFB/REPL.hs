{-# LANGUAGE OverloadedStrings #-}
module FGEFB.REPL
where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified System.Console.Haskeline as Haskeline
import Text.Megaparsec.Error (errorBundlePretty)

import qualified Language.ScrapeScript.AST as Script
import qualified Language.ScrapeScript.Interpreter as Script
import qualified Language.ScrapeScript.Parser as Script

runREPL :: IO ()
runREPL =
  Haskeline.runInputT Haskeline.defaultSettings loop
  where
    loop :: Haskeline.InputT IO ()
    loop = do
      minput <- Haskeline.getInputLine "> "
      case minput of
        Nothing ->
          return ()
        Just "quit" ->
          return ()
        Just input -> do
          let parserResult = Script.parseExpr "" (Text.pack input)
          case parserResult of
            Left err ->
              Haskeline.outputStrLn $ errorBundlePretty err
            Right expr -> do
              Haskeline.outputStrLn $ show expr
              interpreterResult <- liftIO $
                Script.runInterpret Map.empty (Script.eval expr)
              case interpreterResult of
                Left err ->
                  Haskeline.outputStrLn err
                Right result ->
                  Haskeline.outputStrLn . Text.unpack $
                    Script.stringify result
          loop
