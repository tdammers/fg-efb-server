{-# LANGUAGE OverloadedStrings #-}

module FGEFB.XmlUtil
where

import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
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
