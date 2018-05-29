{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
module FM.Core.Search
    ( runSearchRequest
    , SearchConfig(..)
    )
where

import FM.Api.Flight
import FM.Api.Provider.Amadeus
import FM.Core.Types
import FM.Specification.Engine
import FM.Specification.Types

import Control.Logger.Simple
import Control.Monad
import Data.Aeson
import Data.Aeson.TH
import Data.Time.Calendar.WeekDate
import Data.Time.LocalTime
import qualified Data.Text as T
import qualified Data.Vector as V

data SearchConfig
    = SearchConfig
    { sc_trips :: V.Vector TripSpec
    , sc_amadeusKey :: T.Text
    , sc_weekLookAhead :: Int
    } deriving (Show, Eq)

runSearchRequest :: SearchConfig -> IO (V.Vector Trip)
runSearchRequest sc =
    do (currentYear, currentWeek, _) <-
           toWeekDate . localDay . zonedTimeToLocalTime <$> getZonedTime
       let searchWeeks currentCw x
               | x <= 0 = []
               | otherwise =
                     ( currentCw
                      : searchWeeks (advanceCalendarWeek currentCw) (x - 1)
                     )
           weeks =
               searchWeeks (CalendarWeek currentWeek (fromIntegral currentYear)) (sc_weekLookAhead sc)
       weekResults <-
           forM weeks $ \week ->
           forM (sc_trips sc) $ \trip ->
           do logInfo ("Searching for flights in " <> showText week <> " to " <> showText (ts_destination trip))
              results <-
                  handleResults trip <$>
                  runFlightSearch (AmadeusConfig $ sc_amadeusKey sc) (searchRequestFromSpec week trip)
              logInfo ("Found " <> showText (length results) <> " connections")
              pure results
       pure $ V.concat $ map join weekResults

$(deriveJSON defaultOptions{fieldLabelModifier = drop 3} ''SearchConfig)
