{-# LANGUAGE OverloadedStrings #-}

module Main where

import Datasets
import qualified GenericTypes as G

import Data.Aeson
import Data.Aeson.Types
import GHC.Exts (fromList)

import Utils

datasetId = "cygq7AoYo9WCk9y9W"
token = "JB6uoFPmpJPR45jZRBDGXjv4h" :: G.Token --JB6uoFPmpJPR45jZRBDGXjv4h

main :: IO ()
main = do
    setToken "JB6uoFPmpJPR45jZRBDGXjv4h"
    datasets <- listDatasets token Nothing --defaultListsDatasetsParams { limit = Just 5 }
    print datasets
