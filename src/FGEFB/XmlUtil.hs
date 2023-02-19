{-# LANGUAGE OverloadedStrings #-}

module FGEFB.XmlUtil
where

import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Text.XML as XML
import qualified Text.XML.Cursor as XML
import qualified Text.XML.Selectors as XML
import qualified Text.XML.Selectors.Parsers.JQ as XML

uqName :: Text -> XML.Name
uqName t = XML.Name t Nothing Nothing

jqQuery :: Text -> XML.Cursor -> [XML.Cursor]
jqQuery q = XML.match $ XML.jqText' q

jqQuery1 :: Text -> XML.Cursor -> Maybe XML.Cursor
jqQuery1 q = listToMaybe . jqQuery q

textContent :: XML.Node -> Text
textContent (XML.NodeContent t) = t
textContent (XML.NodeElement (XML.Element _ _ children)) = mconcat . map textContent $ children
textContent _ = ""

xmlFragmentToDocument :: XML.Element -> XML.Document
xmlFragmentToDocument docroot =
  XML.Document
    { XML.documentPrologue = prologue
    , XML.documentEpilogue = []
    , XML.documentRoot = docroot
    }
    where
      prologue =
        XML.Prologue
          [ XML.MiscInstruction
              XML.Instruction
                { XML.instructionTarget = "xml-stylesheet"
                , XML.instructionData = "type=\"text/xml\" href=\"/static/style.xsl\""
                }
          ]
          Nothing
          []

