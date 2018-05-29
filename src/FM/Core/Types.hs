{-# LANGUAGE StrictData #-}
module FM.Core.Types where

import Data.Aeson
import Data.Char
import Data.Monoid
import Data.Text.ToFromText
import qualified Data.Text as T

import Data.Time.Calendar.WeekDate

newtype IATACode
    = IATACode { unIATACode :: T.Text }
    deriving (Show, Eq, ToJSON)

instance FromJSON IATACode where
    parseJSON =
        withText "iata_code" $ \t ->
        if T.length t /= 3 || not (T.all (\x -> isAlpha x && isUpper x) t)
        then fail ("Invalid iata code: " <> show t)
        else pure (IATACode t)

newtype USD
    = USD { unUSD :: Int }
    deriving (Show, Eq, Ord, ToJSON, FromJSON)

data DayOfWeek
    = Monday
    | Tuesday
    | Wednesday
    | Thursday
    | Friday
    | Saturday
    | Sunday
    deriving (Eq, Show, Enum, Bounded)

instance ToFromText DayOfWeek where
    toText dow =
        case dow of
          Monday -> "monday"
          Tuesday -> "tuesday"
          Wednesday -> "wednesday"
          Thursday -> "thursday"
          Friday -> "friday"
          Saturday -> "saturday"
          Sunday -> "sunday"

instance ToJSON DayOfWeek where
    toJSON = toJSON . toText

instance FromJSON DayOfWeek where
    parseJSON =
        withText "day_of_week" $ \t ->
        fromText t

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

advanceCalendarWeek :: CalendarWeek -> CalendarWeek
advanceCalendarWeek (CalendarWeek week year) =
    let week' = week + 1
    in if week' == 53
          then case fromWeekDateValid (fromIntegral year) week' 1 of
                 Nothing -> CalendarWeek 1 (year + 1)
                 Just _ -> CalendarWeek week' year
          else CalendarWeek week' year
