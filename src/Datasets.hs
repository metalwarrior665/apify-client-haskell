{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Datasets (
    defaultListsDatasetsParams,
    listDatasets,
    getDataset,
    putItems
) where

import Prelude hiding (id)
import GHC.Generics
import Data.Aeson
import Data.Aeson.Types
import Data.Maybe
import Network.HTTP.Simple
import Data.ByteString hiding (foldl)
import Data.ByteString.Char8 hiding (foldl)
import Control.Exception
import Data.Time.LocalTime

import ApifyRequest
import qualified GenericTypes as G
import Utils

localPath = "/datasets"

data Dataset = Dataset {
    modifiedAt :: ZonedTime,
    createdAt :: ZonedTime,
    id :: String,
    accessedAt :: ZonedTime,
    cleanItemCount :: Maybe Int,
    itemCount :: Int,
    userId :: Maybe String,
    name :: Maybe String, 
    actId :: Maybe String,
    actRunId :: Maybe String
} deriving (Generic, Show)

type Item = Object

instance FromJSON Dataset

-- Dataset collection
data ListDatasetsParams = ListDatasetsParams {
    offset :: Maybe Int,
    limit :: Maybe Int,
    desc :: Maybe Bool,
    unnamed :: Maybe Bool
} deriving (Generic, Show)

instance G.ToQuery ListDatasetsParams

defaultListsDatasetsParams = ListDatasetsParams Nothing Nothing Nothing Nothing

listDatasets :: G.Token -> Maybe ListDatasetsParams -> IO (Either G.ApifyClientError (G.PaginationList Dataset))
listDatasets token params = do
    let req' = parseRequest_ $ basePath ++ "/datasets?token=" ++ token
    let req = case params of    
            (Just params) -> addToRequestQueryString (G.toQuery params) req'
            Nothing       -> req'
    bodyOrErr <- apiRequest req :: IO (Either G.ApifyClientError (G.PaginationList Dataset))
    return $ bodyOrErr --parseBodyOrError

createDataset :: String -> G.Token -> IO (Either G.ApifyClientError Dataset)
createDataset name token = do 
    
    
getDataset :: G.IdOrName -> IO (Either G.ApifyClientError Dataset)
getDataset idOrName = do 
    let url = case idOrName of  (G.Id datasetId)    -> basePath ++ "/datasets/" ++ datasetId
                                (G.Name name token) -> basePath ++ "/datasets/" ++ name ++ "?token=" ++ token
    let req = parseRequest_ url
    dataOrErr <- apiRequest req 
    case dataOrErr of 
        (Right (G.ApifyClientData clientData)) -> let dataset = case (fromJSON clientData) of 
                                                                (Success dataset) -> Just dataset
                                                                _                 -> Nothing 
             in return $ Right $ fromJust dataset
        (Left err) -> return $ Left  err

putItems :: String -> Value -> IO ()
putItems datasetId item = do
    token <- getToken
    let req' = parseRequest_ $ basePath ++ "/datasets/" ++ datasetId ++ "/items?token=" ++ token
    let req
            = setRequestMethod "POST"
            $ setRequestBodyJSON item
            $ req'
    print req
    apiRequest req :: IO (Either G.ApifyClientError Value)
    return ()