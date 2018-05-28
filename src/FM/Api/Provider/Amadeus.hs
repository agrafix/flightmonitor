{-# LANGUAGE RecordWildCards #-}
module FM.Api.Provider.Amadeus
    ( runFlightSearch
    , AmadeusConfig(..)
    )
where

import FM.Api.Flight
import FM.Core.Types

import Control.Lens
import Data.Aeson
import Data.Time.Format
import Network.Connection
import Network.HTTP.Client (ManagerSettings)
import Network.HTTP.Client.TLS
import Network.Wreq
import Safe
import qualified Data.Text as T
import qualified Data.Vector as V

data AmadeusConfig
    = AmadeusConfig
    { ac_apiKey :: T.Text
    } deriving (Show, Eq)

newtype AmadeusResponse
    = AmadeusResponse { unAmadeusResponse :: V.Vector Trip }
    deriving (Show, Eq)

newtype AmadeusItinerary
    = AmadeusItinerary { unAmadeusItinerary :: Itinerary }
    deriving (Show, Eq)

instance FromJSON AmadeusItinerary where
    parseJSON =
        withObject "itinerary" $ \obj ->
        do outbound <- obj .: "outbound"
           outboundFlights <- outbound .: "flights"
           inbound <- obj .: "inbound"
           inboundFlights <- inbound .: "flights"
           pure $ AmadeusItinerary
               Itinerary
               { i_outbound = fmap unAmadeusFlight outboundFlights
               , i_inbound = fmap unAmadeusFlight inboundFlights
               }

newtype AmadeusTrip
    = AmadeusTrip { unAmadeusTrip :: Trip }
    deriving (Show, Eq)

instance FromJSON AmadeusTrip where
    parseJSON =
        withObject "amadeus_trip" $ \obj ->
        do itineraries <- obj .: "itineraries"
           fare <- obj .: "fare"
           totalFare <-
               fare .: "total_price" >>= \unparsedFare ->
               case readMay unparsedFare of
                 Nothing -> fail ("Invalid fare: " ++ show unparsedFare)
                 Just (ok :: Double) -> pure (USD $ round ok)
           pure $ AmadeusTrip
               Trip
               { t_itineraries = fmap unAmadeusItinerary itineraries
               , t_totalFare = totalFare
               }

newtype AmadeusFlight
    = AmadeusFlight { unAmadeusFlight :: Flight }
    deriving (Show, Eq)

instance FromJSON AmadeusFlight where
    parseJSON =
        withObject "amadeus_flight" $ \obj ->
        do f_departure <- obj .: "departs_at"
           f_arrival <- obj .: "arrives_at"
           f_airline <- obj .: "marketing_airline"
           f_flightNumber <- obj .: "flight_number"
           pure $ AmadeusFlight Flight {..}

instance FromJSON AmadeusResponse where
    parseJSON =
        withObject "amadeus_response" $ \obj ->
        do results <- obj .: "results"
           pure $ AmadeusResponse $ V.fromList $ fmap unAmadeusTrip results

managerSettings :: ManagerSettings
managerSettings = mkManagerSettings (TLSSettingsSimple True False False) Nothing

runFlightSearch :: AmadeusConfig -> FlightSearchRequest -> IO (V.Vector Trip)
runFlightSearch config searchRequest =
    do let baseOpts =
               defaults
               & manager .~ Left managerSettings
               & param "apikey" .~ [ac_apiKey config]
               & param "origin" .~ [unIATACode (fsr_origin searchRequest)]
               & param "destination" .~ [unIATACode (fsr_destination searchRequest)]
               & param "departure_date"
                   .~
                   [ T.pack (formatTime defaultTimeLocale "%Y-%m-%d" (fsr_departure searchRequest))
                   ]
               & param "nonstop" .~ [if fsr_nonStop searchRequest then "true" else "false"]
               & param "currency" .~ ["USD"]
               & param "number_of_results" .~ [T.pack (show $ fsr_maxResults searchRequest)]
               & param "adults" .~ ["1"]
           fullOpts =
               case fsr_return searchRequest of
                 Left day ->
                     baseOpts &
                     param "return_date" .~ [T.pack (formatTime defaultTimeLocale "%Y-%m-%d" day)]
                 Right lt ->
                     baseOpts &
                     param "return_by" .~ [T.pack (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M" lt)]
       response <-
           asJSON =<<
           getWith fullOpts "https://api.sandbox.amadeus.com/v1.2/flights/low-fare-search"
       if response ^. responseStatus . statusCode /= 200
          then pure mempty
          else pure $ unAmadeusResponse $ response ^.  responseBody
