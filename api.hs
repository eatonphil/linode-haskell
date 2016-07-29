{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api where

import qualified Request as Req (Linode)
import qualified Response as Rsp (Linode (id),
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

import Prelude hiding (id)

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

getDatacenters :: IO (Data.Maybe.Maybe Rsp.Datacenters)
getDatacenters = get ["/datacenters"]

getDatacenter :: String -> IO (Data.Maybe.Maybe Rsp.Datacenter)
getDatacenter id = get ["/datacenters", id]

getDistributions :: IO (Data.Maybe.Maybe Rsp.Distributions)
getDistributions = get ["/distributions"]

getDistribution :: String -> IO (Data.Maybe.Maybe Rsp.Distribution)
getDistribution id = get ["/distributions", id]

getLinodes :: IO (Data.Maybe.Maybe Rsp.Linodes)
getLinodes = get ["/linodes"]

getLinode :: String -> IO (Data.Maybe.Maybe Rsp.Linode)
getLinode id = get ["/linodes", id]

addLinode :: Req.Linode -> IO (Data.Maybe.Maybe Rsp.Linode)
addLinode linode = post ["/linodes"] linode

editLinode :: Rsp.Linode -> IO (Data.Maybe.Maybe Rsp.Linode)
editLinode linode =
  let i = Rsp.id linode in
  put ["/linodes", i] linode

getServices :: IO (Data.Maybe.Maybe Rsp.Services)
getServices = get ["/services"]

getService :: String -> IO (Data.Maybe.Maybe Rsp.Service)
getService id = get ["/services", id]

getDNSZones :: IO (Data.Maybe.Maybe Rsp.DNSZones)
getDNSZones = get ["/dnszones"]

getDNSZone :: String -> IO (Data.Maybe.Maybe Rsp.DNSZone)
getDNSZone id = get ["/dnszones", id]

getKernels :: IO (Data.Maybe.Maybe Rsp.Kernels)
getKernels = get ["/kernels"]

getKernel :: String -> IO (Data.Maybe.Maybe Rsp.Kernel)
getKernel id = get ["/kernels", id]
