{-# LANGUAGE DeriveDataTypeable #-}

import Data.Data

data ListDatasetsParams = ListDatasetsParams {
    offset :: Maybe Int,
    limit :: Maybe Int,
    desc :: Maybe Bool,
    unnamed :: Maybe Bool
} deriving (Show, Data)

signature :: ListDatasetsParams -> [String]
signature = gmapQ (show . toConstr)

main :: IO ()
main = do
    let obj = ListDatasetsParams { offset = (Just 2), limit = Nothing, desc = Nothing, unnamed = Nothing }
    print $ signature obj
