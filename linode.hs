{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import Data.Aeson
import qualified Data.Maybe (Maybe (Just, Nothing) )

import qualified GHC.Generics (Generic)
import Network.HTTP.Conduit

apiRoot = "https://api.alpha.linode.com/v4"

data ListResponse t = ListResponse { total_results :: Int,
                                     total_pages :: Int,
                                     page :: Int,
                                     list :: [t] } deriving (Show, GHC.Generic.Generic)

data Datacenter = Datacenter { id :: String,
                               datacenter :: String,
                               label :: String } deriving (Show, GHC.Generics.Generic)

instance Data.Aeson.FromJSON Datacenter
instance Data.Aeson.ToJSON Datacenter

data Datacenters = ListResponse Datacenter

instance Data.Aeson.FromJSON Datacenters
instance Data.Aeson.ToJSON Datacenters

getDatacenters :: IO (Data.Maybe.Maybe Datacenters)
getDatacenters = do
  rawJson <- simpleHttp (apiRoot ++ "/datacenters")
  return (decode rawJson :: Data.Maybe.Maybe Datacenters)

data Distribution = Distribution { id :: String,
                                   created :: String,
                                   vendor :: String,
                                   recommended :: Bool,
                                   minimum_image_size :: Int,
                                   x64 :: Bool,
                                   label :: String } deriving (Show, GHC.Generics.Generic)

instance Data.Aeson.FromJSON Distribution
instance Data.Aeson.ToJSON Distribution

data Distributions

main = do
  response <- getDatacenters
  case response of
    (Data.Maybe.Just v) -> print (show v)
    Data.Maybe.Nothing  -> print "Error"
