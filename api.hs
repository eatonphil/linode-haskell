{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api where

import qualified Response as Rsp (Linode,
                                  Linodes,
                                  Service,
                                  Services,
                                  Datacenter,
                                  Datacenters,
                                  Distribution,
                                  Distributions,
                                  DNSZone,
                                  DNSZones,
                                  Kernel,
                                  Kernels)

import Data.Aeson
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 (pack)
import qualified Data.Maybe (Maybe (Just, Nothing))
import Network.HTTP.Conduit
import qualified Network.HTTP.Simple (Request, httpJSON, addRequestHeader, getResponseBody)
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

get :: Data.Aeson.FromJSON a => String -> IO (Data.Maybe.Maybe a)
get endpoint = do
  request <- setAuthHeader
             $ parseRequest_ (Text.Printf.printf "GET %s%s" apiRoot endpoint)
  response <- Network.HTTP.Simple.httpJSON request
  return $ Network.HTTP.Simple.getResponseBody response

getSingle :: Data.Aeson.FromJSON a => String -> String -> IO (Data.Maybe.Maybe a)
getSingle endpoint id = do
  get $ Text.Printf.printf "%s/%s" endpoint id

{-
postData :: Data.Aeson.ToJSON a => Data.Aeson.FromJSON b => String -> a -> IO (Data.Maybe.Mayba b)
postData endpoint json = do
  request <- setAuthHeader
             $ setRequestBodyJSON json
             $ parseRequest_ (Text.Printf.printf "POST %s%s" apiRoot endpoint)
  response <- httpJSON request :: Linode
  _ <- print $ show response
  return (Nothing)
-}

getDatacenters :: IO (Data.Maybe.Maybe Rsp.Datacenters)
getDatacenters = get "/datacenters"

getDatacenter :: String -> IO (Data.Maybe.Maybe Rsp.Datacenter)
getDatacenter id = getSingle "/datacenters" id

getDistributions :: IO (Data.Maybe.Maybe Rsp.Distributions)
getDistributions = get "/distributions"

getDistribution :: String -> IO (Data.Maybe.Maybe Rsp.Distribution)
getDistribution id = getSingle "/distributions" id

getLinodes :: IO (Data.Maybe.Maybe Rsp.Linodes)
getLinodes = get "/linodes"

getLinode :: String -> IO (Data.Maybe.Maybe Rsp.Linode)
getLinode id = getSingle "/linodes" id

getServices :: IO (Data.Maybe.Maybe Rsp.Services)
getServices = get "/services"

getService :: String -> IO (Data.Maybe.Maybe Rsp.Service)
getService id = getSingle "/services" id

getDNSZones :: IO (Data.Maybe.Maybe Rsp.DNSZones)
getDNSZones = get "/dnszones"

getDNSZone :: String -> IO (Data.Maybe.Maybe Rsp.DNSZone)
getDNSZone id = getSingle "/dnszones" id

getKernels :: IO (Data.Maybe.Maybe Rsp.Kernels)
getKernels = get "/kernels"

getKernel :: String -> IO (Data.Maybe.Maybe Rsp.Kernel)
getKernel id = getSingle "/kernels" id
