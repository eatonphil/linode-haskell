{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Aeson
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 (pack)
import qualified Data.Maybe (Maybe (Just, Nothing) )
import qualified GHC.Generics (Generic)
import Network.HTTP.Conduit
import Network.HTTP.Simple
import qualified System.Environment (getEnv)
import qualified Text.Printf (printf)

apiRoot = "https://api.alpha.linode.com/v4"

getToken :: IO String
getToken = System.Environment.getEnv "LINODE_TOKEN"

data Datacenter = Datacenter { id :: String,
                               datacenter :: String,
                               label :: String } deriving (Show, GHC.Generics.Generic)

instance Data.Aeson.FromJSON Datacenter
instance Data.Aeson.ToJSON Datacenter

data Datacenters = Datacenters { total_pages :: Int,
                                 total_results :: Int,
                                 page :: Int,
                                 datacenters :: [Datacenter] } deriving (Show, GHC.Generics.Generic)

instance Data.Aeson.FromJSON Datacenters
instance Data.Aeson.ToJSON Datacenters

getData :: Data.Aeson.FromJSON a => String -> IO (Data.Maybe.Maybe a)
getData endpoint = do
  token <- getToken
  let authorizationValue :: S.ByteString =
        Data.ByteString.Char8.pack
        $ Text.Printf.printf "token %s" token
  let request =
        addRequestHeader "Authorization" authorizationValue
        $ parseRequest_ (Text.Printf.printf "GET %s%s" apiRoot endpoint)
  response <- httpJSON request
  return (getResponseBody response)

getDatacenters :: IO (Data.Maybe.Maybe Datacenters)
getDatacenters = getData "/datacenters"

data Distribution = Distribution { id :: String,
                                   created :: String,
                                   vendor :: String,
                                   recommended :: Bool,
                                   minimum_image_size :: Int,
                                   x64 :: Bool,
                                   label :: String } deriving (Show, GHC.Generics.Generic)

instance Data.Aeson.FromJSON Distribution
instance Data.Aeson.ToJSON Distribution

data Distributions = Distributions { total_pages :: Int,
                                     total_results :: Int,
                                     page :: Int,
                                     distributions :: [Distributions] } deriving (Show, GHC.Generics.Generic)

instance Data.Aeson.FromJSON Distributions
instance Data.Aeson.ToJSON Distributions

getDistributions :: IO (Data.Maybe.Maybe Distributions)
getDistributions = getData "/distributions"

data Linode = Linode { id :: String,
                       label :: String,
                       created :: String,
                       group :: String,
                       label :: String,
                       state :: String,
                       total_transfer :: Int,
                       updated :: String } deriving (Show, GHC.Generics.Generic)

instance Data.Aeson.FromJSON Linode
instance Data.Aeson.ToJSON Linode

data DNSZone = DNSZone { id :: String,
                         dnszone :: String,
                         soa_email :: String,
                         description :: String,
                         refresh_sec :: Int,
                         retry_sec :: Int,
                         expire_sec :: Int,
                         ttl_sec :: Int,
                         status :: String,
                         master_ips :: [String],
                         axfr_ips :: [String],
                         display_group :: String,
                         type :: String } deriving (Show, GHC.Generics.Generic)

instance Data.Aeson.FromJSON DNSZone
instance Data.Aeson.ToJSON DNSZone

data DNSZones = DNSZones { total_pages :: Int,
                           total_results :: Int,
                           page :: Int,
                           dnszones :: [DNSZone] } deriving (Show, GHC.Generics.Generic)

instance Data.Aeson.FromJSON DNSZones
instance Data.Aeson.ToJSON DNSZones

getDNSZones :: IO (Data.Maybe.Maybe DNSZones)
getDNSZones = getData "/dnszones"

data Linodes = Linodes { total_pages :: Int,
                         total_results :: Int,
                         page :: Int,
                         linodes :: [Linode] } deriving (Show, GHC.Generics.Generic)

instance Data.Aeson.FromJSON Linodes
instance Data.Aeson.ToJSON Linodes

getLinodes :: IO (Data.Maybe.Maybe Linodes)
getLinodes = getData "/linodes"

main = do
  response <- getLinodes
  case response of
    Just v -> print $ show v
    Nothing -> print "Error"
