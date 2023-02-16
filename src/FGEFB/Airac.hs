{-# LANGUAGE TemplateHaskell #-}
module FGEFB.Airac
where

import Data.Time
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Aeson.TH
import Data.Text (Text)
import qualified Data.Text as Text

data Airac =
  Airac
    { number :: Text
    , pubdate :: Day
    , valid :: Day
    }
    deriving (Show, Eq, Ord)

$(deriveJSON defaultOptions 'Airac)

-- ident: '2105'
-- date: '2021-03-25'
-- year: '2021'
-- month: '3'
-- month2: '03'
-- monthNameU3: 'MAR'
-- day: '25'
-- day2: '25'

data AiracDetails =
  AiracDetails
    { ident :: Text
    , date :: Text
    , year :: Text
    , month :: Text
    , month2 :: Text
    , monthNameU3 :: Text
    , day :: Text
    , day2 :: Text
    }

$(deriveJSON defaultOptions 'AiracDetails)

findCurrentAiracOn :: Day -> [Airac] -> Maybe Airac
findCurrentAiracOn d airacs = go (reverse airacs)
  where
    go [] = Nothing
    go (x:xs)
      | d >= valid x
      = Just x
      | otherwise
      = go xs

airacDetails :: Airac -> AiracDetails
airacDetails a =
  AiracDetails
    { ident = number a
    , date = tfmt "%Y-%m-%d" (valid a)
    , year = tfmt "%Y" (valid a)
    , month = Text.dropWhile (== '0') $ tfmt "%m" (valid a)
    , month2 = tfmt "%m" (valid a)
    , monthNameU3 = Text.toUpper $ tfmt "%b" (valid a)
    , day = Text.dropWhile (== '0') $ tfmt "%d" (valid a)
    , day2 = tfmt "%d" (valid a)
    }
  where
    tfmt fmt = Text.pack . formatTime defaultTimeLocale fmt
