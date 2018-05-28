module FM.Api.Flight where

import FM.Core.Types

import Data.Time.Calendar
import Data.Time.LocalTime
import qualified Data.Text as T
import qualified Data.Vector as V

data FlightSearchRequest
    = FlightSearchRequest
    { fsr_origin :: IATACode
    , fsr_destination :: IATACode
    , fsr_departure :: Day
    , fsr_return :: Either Day LocalTime
    , fsr_nonStop :: Bool
    , fsr_maxResults :: Int
    } deriving (Show, Eq)

data Flight
    = Flight
    { f_departure :: LocalTime
    , f_arrival :: LocalTime
    , f_airline :: T.Text
    , f_flightNumber :: T.Text
    } deriving (Show, Eq)

data Itinerary
    = Itinerary
    { i_outbound :: V.Vector Flight
    , i_inbound :: V.Vector Flight
    } deriving (Show, Eq)

data Trip
    = Trip
    { t_itineraries :: V.Vector Itinerary
    , t_totalFare :: USD
    } deriving (Show, Eq)
