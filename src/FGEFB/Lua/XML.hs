{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FGEFB.Lua.XML
where

import HsLua as Lua

import FGEFB.Lua.Util
import FGEFB.XmlUtil (xmlFragmentToDocumentNoPrologue, textContent, jqQuery)

import qualified Data.ByteString.Lazy as LBS
import Data.Default (def)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import qualified Text.HTML.DOM as HTML
import qualified Text.XML as XML
import qualified Text.XML.Cursor as XML

moduleXML :: forall e. LuaError e => Module e
moduleXML =
  Module
    "xml"
    "XML DOM functionality"
    -- fields
    []
    -- functions
    [ defun "mkName"
        ### liftPure (\local -> XML.Name local Nothing Nothing)
        <#> parameter peekText "string" "local" "unqualified local name"
        =#> functionResult push "object" "XML Name"
    , defun "mkNameNS"
        ### liftPure2 (\local ns -> XML.Name local (Just ns) Nothing)
        <#> parameter peekText "string" "local" "unqualified local name"
        <#> parameter peekText "string" "namespace" "full XML namespace"
        =#> functionResult push "object" "XML Name"
    , defun "mkQName"
        ### liftPure2 (\prefix local -> XML.Name local Nothing (Just prefix))
        <#> parameter peekText "string" "prefix" "namespace alias / prefix"
        <#> parameter peekText "string" "local" "unqualified local name"
        =#> functionResult push "object" "XML Name"
    , defun "mkQNameNS"
        ### liftPure3 (\prefix local ns -> XML.Name local (Just ns) (Just prefix))
        <#> parameter peekText "string" "prefix" "namespace alias / prefix"
        <#> parameter peekText "string" "local" "unqualified local name"
        <#> parameter peekText "string" "namespace" "full XML namespace"
        =#> functionResult push "object" "XML Name"
    , defun "toname"
        ### liftPure id
        <#> parameter (safepeek :: Peeker e XML.Name) "string" "src" "XML Name"
        =#> functionResult push "object" "XML Name"
    , defun "parseHTML"
        ### liftPure (HTML.parseLBS . LBS.fromStrict)
        <#> parameter peekByteString "string" "src" "HTML source"
        =#> functionResult push "object" "XML Document"
    ]
    -- operations
    []

renderName :: XML.Name -> Text.Text
renderName n =
  mconcat
    [ "XML.Name("
    , maybe "" (\ns -> "{" <> ns <> "}") (XML.nameNamespace n)
    , maybe "" (<> ":") (XML.namePrefix n)
    , XML.nameLocalName n
    , ")"
    ]

typeXMLAttribs :: forall e. LuaError e => DocumentedType e (Map XML.Name Text)
typeXMLAttribs =
  deftype "XML Attribs"
    -- operations
    [ operation Tostring $ defun "__tostring"
        ### liftPure show
        <#> udparam typeXMLAttribs "attributes" "object"
        =#> functionResult pushString "string" "stringified attribute list"
    , operation Index $ defun "__index"
        ### liftPure2 (flip Map.lookup)
        <#> udparam typeXMLAttribs "self" "object"
        <#> parameter (safepeek :: Peeker e XML.Name) "object or string" "key" "XML name to look up"
        =#> functionResult (pushMaybe pushText) "string" "value"
    , operation Newindex $ defun "__newindex"
        ### liftPure3 (\m k v -> Map.insert k v m)
        <#> udparam typeXMLAttribs "self" "object"
        <#> parameter (safepeek :: Peeker e XML.Name) "object or string" "key" "XML name"
        <#> parameter peekText "string" "key" "value"
        =#> functionResult push "object" "new attribute list"
    ]
    -- members
    [ readonly
        "size"
        "number of attributes"
        (pushIntegral, Map.size)
    ]

pushAttribs :: LuaError e => Pusher e (Map XML.Name Text)
pushAttribs = pushUD typeXMLAttribs

peekAttribs :: LuaError e => Peeker e (Map XML.Name Text)
peekAttribs = peekUD typeXMLAttribs

typeXMLName :: LuaError e => DocumentedType e XML.Name
typeXMLName =
  deftype "XML Name"
    -- operations
    [ operation Tostring $ defun "__tostring"
        ### liftPure renderName
        <#> udparam typeXMLName "name" "object"
        =#> functionResult pushText "string" "stringified name"
    ]
    -- members
    [ readonly
        "local"
        "unqualified local name"
        (pushText, XML.nameLocalName)
    , readonly
        "namespace"
        "namespace URI"
        (pushMaybe pushText, XML.nameNamespace)
    , readonly
        "prefix"
        "namespace alias"
        (pushMaybe pushText, XML.namePrefix)
    ]

instance Pushable XML.Name where
  push = pushUD typeXMLName

instance Peekable XML.Name where
  safepeek =
    choice
      [ fmap fromString . peekString
      , peekUD typeXMLName
      ]

renderElement :: XML.Element -> Text.Text
renderElement e =
  let doc = xmlFragmentToDocumentNoPrologue e
  in renderDocumentWith
        def
          { XML.rsXMLDeclaration = False
          , XML.rsPretty = True
          }
        doc

renderDocumentWith :: XML.RenderSettings -> XML.Document -> Text
renderDocumentWith options doc =
  LText.toStrict $
    XML.renderText
      options
      doc

renderDocument :: XML.Document -> Text
renderDocument = renderDocumentWith
  def
    { XML.rsXMLDeclaration = True
    , XML.rsPretty = True
    }

getAttr :: XML.Element -> XML.Name -> Maybe Text
getAttr e name =
  Map.lookup name (XML.elementAttributes e)

typeXMLElement :: LuaError e => DocumentedType e XML.Element
typeXMLElement =
  deftype "XML Element"
    -- operations
    [ operation Tostring $ defun "__tostring"
        ### liftPure renderElement
        <#> udparam typeXMLElement "element" "object"
        =#> functionResult pushText "string" "stringified element"
    ]
    -- members
    [ readonly
        "type"
        "node type"
        (pushText, const "element")
    , readonly
        "name"
        "element name"
        (push, XML.elementName)
    , readonly
        "attributes"
        "element attributes"
        (pushAttribs, XML.elementAttributes)
    , readonly
        "children"
        "element child nodes"
        (pushList push, XML.elementNodes)
    , readonly
        "textContent"
        "element text content"
        (pushText, textContent . XML.NodeElement)
    , method $ defun "attr"
        ### liftPure2 getAttr
        <#> udparam typeXMLElement "element" "object"
        <#> parameter safepeek "string" "nameStr" "unqualified attribute name"
        =#> functionResult (pushMaybe push) "string" "attribute value"
    , method $ defun "query"
        ### liftPure2 (\e selectorStr ->
              jqQuery selectorStr (XML.fromNode (XML.NodeElement e)))
        <#> udparam typeXMLElement "element" "object"
        <#> parameter peekText "string" "selector" "CSS selector"
        =#> functionResult (pushList push) "list" "query results"
    ]

instance Pushable XML.Element where
  push = pushUD typeXMLElement

instance Peekable XML.Element where
  safepeek = peekUD typeXMLElement

renderNode :: XML.Node -> Text
renderNode (XML.NodeElement e) = renderElement e
renderNode (XML.NodeComment comment) = "<!-- " <> comment <> " -->"
renderNode (XML.NodeContent content) = content
renderNode (XML.NodeInstruction _) = "<? ... ?>"

typeXMLNode :: LuaError e => DocumentedType e XML.Node
typeXMLNode = 
  deftype "XML Node"
    -- operations
    [ operation Tostring $ defun "__tostring"
        ### liftPure renderNode
        <#> udparam typeXMLNode "node" "object"
        =#> functionResult pushText "string" "stringified node"
    ]
    -- members
    [ readonly
        "type"
        "node type"
        (pushText, \case
          XML.NodeElement _ -> "element"
          XML.NodeComment _ -> "comment"
          XML.NodeContent _ -> "content"
          XML.NodeInstruction _ -> "instruction"
        )
    , readonly
        "name"
        "node name"
        (pushMaybe push, \case
          XML.NodeElement e -> Just $ XML.elementName e
          _ -> Nothing
        )
    , readonly
        "attributes"
        "node attributes"
        (pushMaybe pushAttribs, \case
          XML.NodeElement e -> Just $ XML.elementAttributes e
          _ -> Nothing
        )
    , readonly
        "children"
        "node child nodes"
        (pushList push, \case
          XML.NodeElement e -> XML.elementNodes e
          _ -> []
        )
    , readonly
        "textContent"
        "text content"
        (pushText, textContent)

    , method $ defun "attr"
        ### liftPure2 (\case
              XML.NodeElement e -> getAttr e
              _ -> const Nothing
            )
        <#> udparam typeXMLNode "node" "object"
        <#> parameter safepeek "string" "nameStr" "unqualified attribute name"
        =#> functionResult (pushMaybe push) "string" "attribute value"

    , method $ defun "query"
        ### liftPure2 (\n selectorStr ->
              jqQuery selectorStr (XML.fromNode n))
        <#> udparam typeXMLNode "node" "object"
        <#> parameter peekText "string" "selector" "CSS selector"
        =#> functionResult (pushList push) "list" "query results"
    ]

instance Pushable XML.Node where
  push = pushUD typeXMLNode

instance Peekable XML.Node where
  safepeek = peekUD typeXMLNode

typeXMLCursor :: LuaError e => DocumentedType e XML.Cursor
typeXMLCursor =
  deftype "XML Cursor"
    -- operations
    [ operation Tostring $ defun "__tostring"
        ### liftPure (\c -> "${" <> (renderNode . XML.node $ c) <> "}")
        <#> udparam typeXMLCursor "cursor" "object"
        =#> functionResult pushText "string" "stringified document"
    ]
    -- members
    [ readonly
        "node"
        "the node that the cursor points to"
        (push, XML.node)
    , method $ defun "query"
        ### liftPure2 (\cursor selectorStr ->
              jqQuery selectorStr cursor)
        <#> udparam typeXMLCursor "cursor" "object"
        <#> parameter peekText "string" "selector" "CSS selector"
        =#> functionResult (pushList push) "list" "query results"

    ]

instance Pushable XML.Cursor where
  push = pushUD typeXMLCursor

instance Peekable XML.Cursor where
  safepeek = peekUD typeXMLCursor


typeXMLDocument :: LuaError e => DocumentedType e XML.Document
typeXMLDocument = 
  deftype "XML Document"
    -- operations
    [ operation Tostring $ defun "__tostring"
        ### liftPure renderDocument
        <#> udparam typeXMLDocument "document" "object"
        =#> functionResult pushText "string" "stringified document"
    ]
    -- members
    [ readonly
        "rootElement"
        "document root element"
        (push, XML.documentRoot)
    , method $ defun "query"
        ### liftPure2 (\doc selectorStr ->
              jqQuery selectorStr (XML.fromDocument doc))
        <#> udparam typeXMLDocument "document" "object"
        <#> parameter peekText "string" "selector" "CSS selector"
        =#> functionResult (pushList push) "list" "query results"
    ]

instance Pushable XML.Document where
  push = pushUD typeXMLDocument

instance Peekable XML.Document where
  safepeek = peekUD typeXMLDocument
