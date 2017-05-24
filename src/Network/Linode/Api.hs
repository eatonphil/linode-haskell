{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Linode.Api where

import qualified Network.Linode.Response.Region as Region (Region, Regions)
import qualified Network.Linode.Response.Distribution as Distribution (Distribution, Distributions)

import Prelude

import Data.Aeson
import qualified Data.List (intercalate)
import qualified Data.ByteString as S (ByteString)
import qualified Data.ByteString.Char8 (pack)
import qualified Data.Maybe (Maybe (Just, Nothing))
import qualified Network.HTTP.Conduit (parseRequest_)
import qualified Network.HTTP.Simple (Request, httpJSON, addRequestHeader, getResponseBody, setRequestBodyJSON)
import qualified System.Environment (getEnv)
import qualified Text.Printf (printf)

apiRoot :: String
apiRoot = "https://api.alpha.linode.com/v4"

setAuthHeader :: Network.HTTP.Simple.Request -> IO Network.HTTP.Simple.Request
setAuthHeader request = do
  token <- System.Environment.getEnv "LINODE_TOKEN"
  let authorizationValue :: S.ByteString =
        Data.ByteString.Char8.pack
        $ Text.Printf.printf "token %s" token
  return $ Network.HTTP.Simple.addRequestHeader "Authorization" authorizationValue request

getJson :: Data.Aeson.FromJSON a => String -> [String] -> IO (Data.Maybe.Maybe a)
getJson method urlFragments = do
  let endpoint = Data.List.intercalate "/" urlFragments
  let requestString =
        Text.Printf.printf "%s %s%s" method apiRoot endpoint
  request <- setAuthHeader
             $ Network.HTTP.Conduit.parseRequest_ requestString
  response <- Network.HTTP.Simple.httpJSON request
  return $ Network.HTTP.Simple.getResponseBody response

get :: Data.Aeson.FromJSON a => [String] -> IO (Data.Maybe.Maybe a)
get = getJson "GET"

delete :: Data.Aeson.FromJSON a => [String] -> IO (Data.Maybe.Maybe a)
delete = getJson "DELETE"

sendJson :: Data.Aeson.ToJSON a => Data.Aeson.FromJSON b => String -> [String] -> a -> IO (Data.Maybe.Maybe b)
sendJson method urlFragments object = do
  let endpoint = Data.List.intercalate "/" urlFragments
  let requestString =
        Text.Printf.printf "%s %s%s" method apiRoot endpoint
  request <- setAuthHeader
             $ Network.HTTP.Simple.setRequestBodyJSON object
             $ Network.HTTP.Conduit.parseRequest_ requestString
  response <- Network.HTTP.Simple.httpJSON request
  return $ Network.HTTP.Simple.getResponseBody response

post :: Data.Aeson.ToJSON a => Data.Aeson.FromJSON b => [String] -> a -> IO (Data.Maybe.Maybe b)
post = sendJson "POST"

put :: Data.Aeson.ToJSON a => Data.Aeson.FromJSON b => [String] -> a -> IO (Data.Maybe.Maybe b)
put = sendJson "PUT"

getRegions :: IO (Data.Maybe.Maybe Region.Regions)
getRegions = get ["/regions"]

getRegion :: String -> IO (Data.Maybe.Maybe Region.Region)
getRegion id = get ["/regions", id]

getDistributions :: IO (Data.Maybe.Maybe Distribution.Distributions)
getDistributions = get ["/linode/distributions"]

getDistributionsRecommended :: IO (Data.Maybe.Maybe Distribution.Distributions)
getDistributionsRecommended = get ["/linode/distributions/recommended"]

getDistribution :: String -> IO (Data.Maybe.Maybe Distribution.Distribution)
getDistribution id = get ["/linode/distributions", id]
