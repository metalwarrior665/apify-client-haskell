module Utils where

import System.Environment

import GenericTypes

getToken :: IO Token
getToken = do
    token <- getEnv "APIFY_TOKEN"
    return token

setToken :: Token -> IO ()
setToken = setEnv "APIFY_TOKEN"