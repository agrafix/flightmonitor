module Main where

import FM.Api.Flight
import FM.Core.Search

import Control.Logger.Simple
import Control.Monad
import Data.Yaml
import System.Environment
import qualified Data.Text as T
import qualified Data.Text.IO as T

main :: IO ()
main =
    withGlobalLogging (LogConfig Nothing True) $
    do args <- getArgs
       case args of
         [fileName] ->
             do decodeResult <-
                    decodeFileEither fileName
                case decodeResult of
                  Left errMsg ->
                      logError ("Failed to decode " <> T.pack fileName <> ": " <> showText errMsg)
                  Right value ->
                      do trips <- runSearchRequest value
                         forM_ trips $ \trip ->
                             T.putStrLn (prettyTrip trip)
         _ -> logInfo "Usage: ./flightmonitor config.yaml"
