{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
module FM.Specification.Types where

import FM.Core.Types

import Data.Aeson
import Data.Aeson.TH
import Data.Char
import Data.Time.LocalTime

data ReturnSpec
    = RsLength Int
    | RsReturnDay DayOfWeek TimeOfDay
    deriving (Show, Eq)

data DepartureSpec
    = DepartureSpec
    { ds_dayOfWeek :: DayOfWeek
    , ds_time :: Maybe TimeOfDay
    } deriving (Show, Eq)

data TripSpec
    = TripSpec
    { ts_origin :: IATACode
    , ts_destination :: IATACode
    , ts_departure :: DepartureSpec
    , ts_return :: ReturnSpec
    , ts_nonStop :: Bool
    , ts_maxPriceUSD :: Maybe USD
    } deriving (Show, Eq)

$(deriveJSON defaultOptions{sumEncoding=ObjectWithSingleField, constructorTagModifier = drop 2 . map toLower} ''ReturnSpec)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 3} ''DepartureSpec)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 3} ''TripSpec)
