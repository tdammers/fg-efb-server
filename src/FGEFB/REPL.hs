{-# LANGUAGE OverloadedStrings #-}
module FGEFB.REPL
where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.IORef
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified System.Console.Haskeline as Haskeline
import Text.Megaparsec.Error (errorBundlePretty)
import Text.Megaparsec (initialPos)
import Control.Exception (displayException)

import qualified Language.ScrapeScript.AST as Script
import qualified Language.ScrapeScript.Interpreter as Script
import qualified Language.ScrapeScript.Parser as Script

newtype ReplState
  = ReplState
      { replShowAST :: Bool
      }

defReplState :: ReplState
defReplState =
  ReplState
    { replShowAST = False
    }

onOff :: Bool -> String
onOff True = "on"
onOff False = "off"

runREPL :: IO ()
runREPL = do
  stateVar <- newIORef defReplState

  let loop :: Haskeline.InputT IO ()
      loop = do
        minput <- Haskeline.getInputLine "> "
        case minput of
          Nothing ->
            return ()
          Just (':':command) ->
            case words command of
              ["quit"] ->
                return ()
              ["dump-ast", "on"] -> do
                liftIO . modifyIORef stateVar $ \s -> s { replShowAST = True }
                loop
              ["dump-ast", "off"] -> do
                liftIO . modifyIORef stateVar $ \s -> s { replShowAST = False }
                loop
              ["dump-ast"] -> do
                on <- replShowAST <$> liftIO (readIORef stateVar)
                Haskeline.outputStrLn $ "Dump AST: " ++ onOff on
                loop
              _ -> do
                Haskeline.outputStrLn $ "Unknown command: :" ++ command
                loop
          Just input -> do
            let parserResult = Script.parseExpr "" (Text.pack input)
            case parserResult of
              Left err ->
                Haskeline.outputStrLn $ errorBundlePretty err
              Right expr -> do
                state <- liftIO $ readIORef stateVar
                when (replShowAST state) $ do
                  Haskeline.outputStrLn $ show expr
                interpreterResult <- liftIO $
                  Script.runInterpret (initialPos "") Map.empty (Script.eval expr)
                case interpreterResult of
                  Left err ->
                    Haskeline.outputStrLn (displayException err)
                  Right result ->
                    Haskeline.outputStrLn . Text.unpack $
                      Script.stringify result
            loop

  Haskeline.runInputT Haskeline.defaultSettings loop
