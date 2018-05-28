{-# LANGUAGE StrictData #-}
module FM.Core.Types where

import qualified Data.Text as T

newtype IATACode
    = IATACode { unIATACode :: T.Text }
    deriving (Show, Eq)

newtype USD
    = USD { unUSD :: Int }
    deriving (Show, Eq, Ord)

data DayOfWeek
    = Monday
    | Tuesday
    | Wednesday
    | Thursday
    | Friday
    | Saturday
    | Sunday
    deriving (Eq, Show)

dayOfWeekToInt :: DayOfWeek -> Int
dayOfWeekToInt dow =
    case dow of
      Monday -> 1
      Tuesday -> 2
      Wednesday -> 3
      Thursday -> 4
      Friday -> 5
      Saturday -> 6
      Sunday -> 7

data CalendarWeek
    = CalendarWeek
    { cw_week :: Int -- ^ 1-52
    , cw_year :: Int
    } deriving (Show, Eq)
