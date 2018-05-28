module Main where

import FM.Core.Search

main :: IO ()
main =
    do res <- runSearchRequest
       print res
