{-#LANGUAGE OverloadedStrings #-}
module FGEFB.Airac
where

import qualified Data.Aeson as JSON
import Data.Aeson ( (.:), (.:?), (.!=) )
import Data.Time
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Read (readMaybe)

data AiracCycle =
  AiracCycle
    { airacIdent :: Int
    , airacDate :: Day
    }
    deriving (Show)

instance JSON.FromJSON AiracCycle where
  parseJSON = JSON.withObject "AiracCycle" $ \obj -> do
    ident <- obj .: "ident"
    dateStr <- obj .: "date"
    date <- case Text.splitOn "-" dateStr of
      [yearStr, monthStr, dayStr] -> do
        year <- maybe (fail "invalid year") return $ readMaybe (Text.unpack yearStr)
        month <- maybe (fail "invalid month") return $ readMaybe (Text.unpack monthStr)
        day <- maybe (fail "invalid day") return $ readMaybe (Text.unpack dayStr)
        return $ fromGregorian year month day
      _ -> fail "Invalid airac date"
    return $ AiracCycle ident date
