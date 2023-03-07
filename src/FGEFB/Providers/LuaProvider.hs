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

luaThrowStatus :: Lua.LuaError e => Lua.LuaE e Lua.Status -> Lua.LuaE e ()
luaThrowStatus action = do
  result <- action
  when (result /= Lua.OK) $ do
    msg <- Lua.peek 1
    Lua.failLua $ show result ++ ": " ++ msg

peekMaybe :: Lua.LuaError e => Lua.Peeker e a -> Lua.Peeker e (Maybe a)
peekMaybe peeker =
  Lua.choice
    [ fmap (const Nothing) . Lua.peekNil
    , fmap Just . peeker
    ]

pushMaybe :: Lua.LuaError e => (a -> Lua.LuaE e ()) -> Maybe a -> Lua.LuaE e ()
pushMaybe pushJust (Just x) = pushJust x
pushMaybe _ Nothing = Lua.pushnil

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

-- * Pushing and peeking XML

data IDNode
  = IDNodeElement
  | IDNodeInstruction
  | IDNodeContent
  | IDNodeComment
  deriving (Show, Read, Enum, Ord, Eq, Bounded)

instance Lua.Pushable IDNode where
  push = Lua.pushIntegral . fromEnum

instance Lua.Peekable IDNode where
  safepeek = fmap toEnum . Lua.peekIntegral

pushXMLName :: Lua.LuaError e => XML.Name -> Lua.LuaE e ()
pushXMLName =
  Lua.pushAsTable
    [ ("local", Lua.pushText . XML.nameLocalName)
    , ("prefix", pushMaybe Lua.pushText . XML.namePrefix)
    , ("namespace", pushMaybe Lua.pushText . XML.nameNamespace)
    ]

instance Lua.Pushable XML.Name where
  push = pushXMLName

peekXMLName :: Lua.LuaError e => Lua.Peeker e XML.Name
peekXMLName idx = do
  local <- Lua.peekFieldRaw Lua.peekText "local" idx
  prefix <- Lua.peekFieldRaw (peekMaybe Lua.peekText) "prefix" idx
  ns <- Lua.peekFieldRaw (peekMaybe Lua.peekText) "namespace" idx
  return $ XML.Name local prefix ns

instance Lua.Peekable XML.Name where
  safepeek = peekXMLName
  
pushXMLElement :: Lua.LuaError e => XML.Element -> Lua.LuaE e ()
pushXMLElement =
  Lua.pushAsTable
    [ ("name", pushXMLName . XML.elementName)
    , ("attr", Lua.pushMap pushXMLName Lua.pushText . XML.elementAttributes)
    , ("nodes", Lua.pushList pushXMLNode . XML.elementNodes)
    , ("what", Lua.push . const IDNodeElement)
    ]

peekXMLElement :: Lua.LuaError e => Lua.Peeker e XML.Element
peekXMLElement idx = do
  name <- Lua.peekFieldRaw peekXMLName "name" idx
  attribs <- Lua.peekFieldRaw (Lua.peekMap peekXMLName Lua.peekText) "attr" idx
  nodes <- Lua.peekFieldRaw (Lua.peekList peekXMLNode) "nodes" idx
  return $ XML.Element name attribs nodes

pushXMLInstruction :: Lua.LuaError e => XML.Instruction -> Lua.LuaE e ()
pushXMLInstruction =
  Lua.pushAsTable
    [ ("target", Lua.pushText . XML.instructionTarget)
    , ("data", Lua.pushText . XML.instructionData)
    , ("what", Lua.push . const IDNodeInstruction)
    ]

peekXMLInstruction :: Lua.LuaError e => Lua.Peeker e XML.Instruction
peekXMLInstruction idx = do
  target <- Lua.peekFieldRaw Lua.peekText "target" idx
  data_ <- Lua.peekFieldRaw Lua.peekText "data" idx
  return $ XML.Instruction target data_

pushXMLNode :: Lua.LuaError e => XML.Node -> Lua.LuaE e ()
pushXMLNode (XML.NodeContent content) =
  Lua.pushText content
pushXMLNode (XML.NodeElement element) =
  pushXMLElement element
pushXMLNode (XML.NodeInstruction instruction) =
  pushXMLInstruction instruction
pushXMLNode (XML.NodeComment comment) =
  pushXMLComment comment

pushXMLComment :: Lua.LuaError e => Text -> Lua.LuaE e ()
pushXMLComment =
  Lua.pushAsTable
    [ ("what", Lua.push . const IDNodeComment)
    , ("comment", Lua.pushText)
    ]

peekXMLNode :: Lua.LuaError e => Lua.Peeker e XML.Node
peekXMLNode =
  Lua.choice
    [ fmap XML.NodeContent . Lua.peekText
    , peekTaggedXMLNode
    ]
  where
    peekTaggedXMLNode idx = do
      idNode <- Lua.peekFieldRaw (Lua.liftLua . Lua.peek) "what" idx
      case idNode of
        IDNodeElement -> XML.NodeElement <$> peekXMLElement idx
        IDNodeComment -> XML.NodeComment <$> Lua.peekFieldRaw Lua.peekText "comment" idx
        IDNodeInstruction -> XML.NodeInstruction <$> peekXMLInstruction idx
        IDNodeContent -> XML.NodeContent <$> Lua.peekFieldRaw Lua.peekText "content" idx

pushXMLDocument :: Lua.LuaError e => XML.Document -> Lua.LuaE e ()
pushXMLDocument =
  Lua.pushAsTable
    [ ("prologue", pushXMLPrologue . XML.documentPrologue)
    , ("root", pushXMLElement . XML.documentRoot)
    , ("epilogue", Lua.pushList pushXMLMiscellaneous . XML.documentEpilogue)
    ]

instance Lua.Pushable XML.Document where
  push = pushXMLDocument

pushXMLMiscellaneous :: Lua.LuaError e => XML.Miscellaneous -> Lua.LuaE e ()
pushXMLMiscellaneous (XML.MiscInstruction instruction) = pushXMLInstruction instruction
pushXMLMiscellaneous (XML.MiscComment comment) = pushXMLComment comment

pushXMLPrologue :: Lua.LuaError e => XML.Prologue -> Lua.LuaE e ()
pushXMLPrologue =
  Lua.pushAsTable
    [ ("before", Lua.pushList pushXMLMiscellaneous . XML.prologueBefore)
    , ("doctype", pushMaybe pushXMLDoctype . XML.prologueDoctype)
    , ("after", Lua.pushList pushXMLMiscellaneous . XML.prologueAfter)
    ]

pushXMLDoctype :: Lua.LuaError e => XML.Doctype -> Lua.LuaE e ()
pushXMLDoctype =
  Lua.pushAsTable
    [ ("name", Lua.pushText . XML.doctypeName)
    , ("id", pushMaybe pushXMLExternalID . XML.doctypeID)
    ]

pushXMLExternalID :: Lua.LuaError e => XML.ExternalID -> Lua.LuaE e ()
pushXMLExternalID (XML.SystemID a) =
  Lua.pushList Lua.pushText ["SYSTEM", a]
pushXMLExternalID (XML.PublicID a b) = do
  Lua.pushList Lua.pushText ["PUBLIC", a, b]

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

    parseHTML :: ByteString -> Lua.Lua XML.Document
    parseHTML = return . HTML.parseLBS . LBS.fromStrict

    makeXMLName :: Text -> Text -> Text -> Lua.Lua XML.Name
    makeXMLName local ns prefix =
      return $ XML.Name local (Just ns) (Just prefix)

    runLua :: Lua.Name -> Text -> Lua.Peeker Lua.Exception a -> IO a
    runLua funcname pathText peeker = do
      luaOutput <- Lua.runEither $ do
        Lua.openlibs
        forM_ [minBound .. maxBound :: IDNode] $ \i -> do
          Lua.push i
          Lua.setglobal (Lua.Name . encodeUtf8 . Text.pack . show $ i)
        Lua.registerHaskellFunction "httpGET" httpGET
        Lua.registerHaskellFunction "parseHTML" parseHTML
        Lua.registerHaskellFunction "makeXMLName" makeXMLName
        luaThrowStatus $ Lua.dofileTrace scriptFilename
        Lua.getglobal funcname
        Lua.push pathText
        luaThrowStatus $ Lua.pcallTrace 1 1
        Lua.force =<< Lua.runPeeker peeker Lua.top
      either throwIO return luaOutput

    unpackParam :: Text -> Text
    unpackParam = decodeUtf8 . urlDecode True . encodeUtf8
