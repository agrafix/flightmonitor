module FM.Api.Flight where

import FM.Core.Types

import Data.Monoid
import Data.Time.Calendar
import Data.Time.Format
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

prettyTrip :: Trip -> T.Text
prettyTrip trip =
    "# Price: " <> T.pack (show (unUSD (t_totalFare trip))) <> " USD \n"
    <> T.intercalate "\n" (V.toList $ fmap prettyItinerary (t_itineraries trip))

prettyItinerary :: Itinerary -> T.Text
prettyItinerary it =
    "Outbound: \n"
    <> flightList i_outbound
    <> "\nInbound: \n"
    <> flightList i_inbound
    where
      flightList get =
          T.intercalate "\n" (V.toList $ fmap prettyFlight (get it))

prettyFlight :: Flight -> T.Text
prettyFlight f =
     f_airline f <> " " <> f_flightNumber f <> ": "
    <> mkTime (f_departure f) <> " -> " <> mkTime (f_arrival f)
    where
      mkTime =
          T.pack . formatTime defaultTimeLocale "%Y-%m-%d %H:%M"
