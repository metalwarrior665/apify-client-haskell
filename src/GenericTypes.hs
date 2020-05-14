{-# LANGUAGE OverloadedStrings, DefaultSignatures, NamedFieldPuns, DeriveGeneric, FlexibleContexts, FlexibleInstances, TypeOperators #-}

module GenericTypes where

import GHC.Generics
import Data.ByteString (ByteString)
import Data.String
import Data.Aeson
import Data.Aeson.Types
import Data.Maybe
import Network.HTTP.Simple
import Network.HTTP.Types.Status

type Token = String
data IdOrName = Id String | Name String Token

data ApifyErrorDescription = ApifyErrorDescription {
    errorType :: String,
    errorMessage :: String
} deriving (Show)

instance FromJSON ApifyErrorDescription where
    parseJSON = withObject "response" $ \response -> do
        resp <- response .: "error"
        return resp

data ApifyClientError = ApifyClientError {
    status :: Status,
    apifyErrorDescription :: Maybe ApifyErrorDescription -- Nothing on >= 500 status
} deriving (Show)

-- So far this doesn't work for CSV etc. so it will have to be rewritten
newtype ApifyClientData = ApifyClientData Value deriving Show

instance FromJSON ApifyClientData where
    parseJSON = withObject "response" $ \response -> do
        clientData <- response .: "data"
        return (ApifyClientData clientData)
                 
-- PaginationList
data PaginationList a = PaginationList {
    total :: Int,
    offset :: Int,
    limit :: Int,
    desc :: Bool,
    count :: Int,
    items :: [a]
} deriving (Show, Generic)

instance (FromJSON a) => FromJSON (PaginationList a) 

-- This is some kind of generic trickery so we can make generic query strings
-- Generic toQuery
class ToQueryValue a where
    toQueryValue :: a -> ByteString
    default toQueryValue :: Show a => a -> ByteString
    toQueryValue = fromString . show

instance ToQueryValue Int
instance ToQueryValue Bool where
    toQueryValue bool = case bool of
        True -> "1"
        False -> "0" 
instance ToQueryValue [Char] where
    toQueryValue = fromString

class GToQuery f where
    gToQuery :: f a -> Query

instance (GToQuery a, GToQuery b) => GToQuery (a :*: b) where
    gToQuery (a :*: b) = gToQuery a ++ gToQuery b

instance GToQuery a => GToQuery (M1 D c a) where
    gToQuery (M1 x) = gToQuery x

instance GToQuery a => GToQuery (M1 C c a) where
    gToQuery (M1 x) = gToQuery x

instance (Selector c, ToQueryValue a) => GToQuery (M1 S c (K1 i (Maybe a))) where
    gToQuery s@(M1 (K1 x)) = [(toQueryValue (selName s), fmap toQueryValue x)]

class ToQuery a where
    toQuery :: a -> Query
    default toQuery :: (Generic a, GToQuery (Rep a)) => a -> Query
    toQuery = gToQuery . from

