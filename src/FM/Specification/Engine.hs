module FM.Specification.Engine
    ( searchRequestFromSpec, handleResults )
where

import FM.Api.Flight
import FM.Core.Types
import FM.Specification.Types

import Data.Time.Calendar
import Data.Time.Calendar.WeekDate
import Data.Time.LocalTime
import qualified Data.Vector as V

mkDepartureDay :: CalendarWeek -> DepartureSpec -> Day
mkDepartureDay cw departureSpec =
    fromWeekDate (fromIntegral $ cw_year cw) (cw_week cw) day
    where
      day =
          dayOfWeekToInt $ ds_dayOfWeek departureSpec

mkReturn :: CalendarWeek -> DepartureSpec -> ReturnSpec -> Either Day LocalTime
mkReturn cw ds rs =
    case rs of
      RsLength dayCount ->
          Left $ addDays (fromIntegral dayCount) departureDay
      RsReturnDay dow timeOfDay ->
          let dayDiffOrig =
                  dayOfWeekToInt dow - dayOfWeekToInt (ds_dayOfWeek ds)
              dayDiff =
                  if dayDiffOrig < 0
                  then dayDiffOrig + 7
                  else dayDiffOrig
              returnDay = addDays (fromIntegral dayDiff) departureDay
          in Right LocalTime { localDay = returnDay, localTimeOfDay = timeOfDay }
    where
      departureDay =
          mkDepartureDay cw ds

searchRequestFromSpec :: CalendarWeek -> TripSpec -> FlightSearchRequest
searchRequestFromSpec cw tripSpec =
    FlightSearchRequest
    { fsr_origin = ts_origin tripSpec
    , fsr_destination = ts_destination tripSpec
    , fsr_nonStop = ts_nonStop tripSpec
    , fsr_maxResults = 25
    , fsr_departure = mkDepartureDay cw (ts_departure tripSpec)
    , fsr_return = mkReturn cw (ts_departure tripSpec) (ts_return tripSpec)
    }

handleResults :: TripSpec -> V.Vector Trip -> V.Vector Trip
handleResults ts =
    priceFilter
    where
      priceFilter =
          case ts_maxPriceUSD ts of
            Nothing -> id
            Just maxPrice -> V.filter (\t -> t_totalFare t <= maxPrice)
