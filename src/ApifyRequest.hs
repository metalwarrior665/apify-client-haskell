{-# LANGUAGE OverloadedStrings, NamedFieldPuns  #-}

module ApifyRequest (
    apiRequest,
    parseBodyOrError,
    basePath
) where

import Network.HTTP.Simple
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString
import Control.Watchdog
import Control.Exception
import Data.Maybe (fromJust)

import GenericTypes

basePath = "https://api.apify.com/v2"

sendRetriableRequest :: Request -> IO (Either String (Response ByteString))
sendRetriableRequest req = do 
    response <- httpBS req
    let status = getResponseStatusCode response
    return $ if status >= 500 || status == 429
        then Left $ "Response status " ++ show status
        else Right response

watchedRequest :: Request -> IO (Either String (Response ByteString))
watchedRequest req = watchdog $ do 
    setInitialDelay $ 100000
    setMaximumRetries 10     
    setLoggingAction defaultLogger
    watchImpatiently (sendRetriableRequest req)

backedoffRequest :: Request -> IO (Response ByteString)
backedoffRequest req = do 
    watchedResponse <- watchedRequest req
    case watchedResponse of 
        (Right resp) -> return resp 
        (Left err)   -> throw (userError $ "Failed after 10 retries: " ++ err)

apiRequest :: FromJSON a => Request -> IO (Either ApifyClientError a)
apiRequest req = do
    response <- backedoffRequest req
    let status = getResponseStatus response
    let code = getResponseStatusCode response
    let body = getResponseBody response
    case code of 
        200 ->  let clientData = fromJust $ decodeStrict body
                in return $ Right $ parseBodyOrError clientData
        _   ->  let apifyErrorDesc = if code >= 500 then Nothing else decodeStrict body :: Maybe ApifyErrorDescription
                    clientErr = ApifyClientError { status, apifyErrorDescription = apifyErrorDesc}
                in return $ Left (clientErr)

parseBodyOrError :: FromJSON a => ApifyClientData ->  a
parseBodyOrError (ApifyClientData unparsedData) = 
    let parsedData = case (fromJSON unparsedData) of 
                            (Success parsed) -> Just parsed
                            (Error err)      -> Nothing
    in fromJust parsedData

