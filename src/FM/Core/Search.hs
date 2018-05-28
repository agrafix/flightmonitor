module FM.Core.Search where

import FM.Api.Flight
import FM.Api.Provider.Amadeus
import FM.Core.Types
import FM.Specification.Engine
import FM.Specification.Types

import Data.Time.LocalTime
import qualified Data.Vector as V

sampleSpec :: TripSpec
sampleSpec =
    TripSpec
    { ts_origin = IATACode "SFO"
    , ts_destination = IATACode "HNL"
    , ts_departure = DepartureSpec Friday Nothing
    , ts_return = RsReturnDay Monday (TimeOfDay 7 0 0)
    , ts_nonStop = True
    , ts_maxPriceUSD = Just (USD 700)
    }

searchRequest :: FlightSearchRequest
searchRequest = searchRequestFromSpec (CalendarWeek 23 2018) sampleSpec

runSearchRequest :: IO (V.Vector Trip)
runSearchRequest =
    handleResults sampleSpec <$>
    runFlightSearch (AmadeusConfig "--REDACTED--") searchRequest
