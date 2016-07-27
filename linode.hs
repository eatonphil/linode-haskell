{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Applicative
import Data.Aeson
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 (pack)
import qualified Data.Maybe (Maybe (Just, Nothing) )
import qualified GHC.Generics (Generic)
import Network.HTTP.Conduit
import Network.HTTP.Simple
import qualified System.Environment (getEnv)
import qualified Text.Printf (printf)

apiRoot :: String
apiRoot = "https://api.alpha.linode.com/v4"

getToken :: IO String
getToken = System.Environment.getEnv "LINODE_TOKEN"

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
  return $ getResponseBody response

getSingle :: Data.Aeson.FromJSON a => String -> String -> IO (Data.Maybe.Maybe a)
getSingle endpoint id = do
  let url :: String = Text.Printf.printf "%s/%s" endpoint id
  getData url

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

getDatacenters :: IO (Data.Maybe.Maybe Datacenters)
getDatacenters = getData "/datacenters"

getDatacenter :: String -> IO (Data.Maybe.Maybe Datacenter)
getDatacenter id = getSingle "/datacenters" id

data Distribution = Distribution { id :: String,
                                   created :: String,
                                   vendor :: String,
                                   recommended :: Bool,
                                   minimum_storage_size :: Int,
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

getDistribution :: String -> IO (Data.Maybe.Maybe Distribution)
getDistribution id = getSingle "/distributions" id

data LinodeAlert = LinodeAlert { enabled :: Bool,
                                 threshold :: Int } deriving (Show, GHC.Generics.Generic)

instance Data.Aeson.FromJSON LinodeAlert
instance Data.Aeson.ToJSON LinodeAlert

data LinodeAlerts = LinodeAlerts { cpu :: LinodeAlert,
                                   io :: LinodeAlert,
                                   transfer_in :: LinodeAlert,
                                   transfer_out :: LinodeAlert,
                                   transfer_quota :: LinodeAlert } deriving (Show, GHC.Generics.Generic)

instance Data.Aeson.FromJSON LinodeAlerts
instance Data.Aeson.ToJSON LinodeAlerts

data LinodeBackupsSchedule = LinodeBackupsSchedule { day :: Data.Maybe.Maybe String,
                                                     window :: Data.Maybe.Maybe String } deriving (Show, GHC.Generics.Generic)

instance Data.Aeson.FromJSON LinodeBackupsSchedule where
  parseJSON (Data.Aeson.Object v) = do
    day :: Data.Maybe.Maybe String <- optional $ v .: "day" >>= Data.Aeson.parseJSON
    window :: Data.Maybe.Maybe String <- optional $ v .: "window" >>= Data.Aeson.parseJSON
    return $ LinodeBackupsSchedule { day=day, window=window }
instance Data.Aeson.ToJSON LinodeBackupsSchedule

data LinodeBackupsLastBackup = LinodeBackupsLastBackup { create_dt :: String,
                                                         duration :: Int,
                                                         finish_dt :: String,
                                                         id :: String,
                                                         message :: String,
                                                         status :: String } deriving (Show, GHC.Generics.Generic)

instance Data.Aeson.FromJSON LinodeBackupsLastBackup
instance Data.Aeson.ToJSON LinodeBackupsLastBackup

data LinodeBackups = LinodeBackups { enabled :: Bool,
                                     schedule :: LinodeBackupsSchedule,
                                     last_backup :: Data.Maybe.Maybe LinodeBackupsLastBackup } deriving (Show, GHC.Generics.Generic)

instance Data.Aeson.FromJSON LinodeBackups where
    parseJSON (Data.Aeson.Object v) = do
    enabled :: Bool <- v .: "enabled" >>= Data.Aeson.parseJSON
    schedule :: LinodeBackupsSchedule <- v .: "schedule" >>= Data.Aeson.parseJSON
    last_backup :: Data.Maybe.Maybe LinodeBackupsLastBackup <- optional $ v .: "last_backup" >>= Data.Aeson.parseJSON
    return $ LinodeBackups { enabled=enabled, schedule=schedule, last_backup=last_backup }
instance Data.Aeson.ToJSON LinodeBackups

data LinodeIPAddressesPrivate = LinodeIPAddressesPrivate { ipv4 :: [String],
                                                           link_local :: String } deriving (Show, GHC.Generics.Generic)

instance Data.Aeson.FromJSON LinodeIPAddressesPrivate
instance Data.Aeson.ToJSON LinodeIPAddressesPrivate

data LinodeIPAddressesPublic = LinodeIPAddressesPublic { failover :: [String],
                                                         ipv4 :: [String],
                                                         ipv6 :: String } deriving (Show, GHC.Generics.Generic)

instance Data.Aeson.FromJSON LinodeIPAddressesPublic
instance Data.Aeson.ToJSON LinodeIPAddressesPublic

data LinodeIPAddresses = LinodeIPAddresses { private :: LinodeIPAddressesPrivate,
                                             public :: LinodeIPAddressesPublic } deriving (Show, GHC.Generics.Generic)

instance Data.Aeson.FromJSON LinodeIPAddresses
instance Data.Aeson.ToJSON LinodeIPAddresses

data Linode = Linode { id :: String,
                       alerts :: LinodeAlerts,
                       backups :: LinodeBackups,
                       created :: String,
                       datacenter :: Datacenter,
                       distribution :: Distribution,
                       group :: String,
                       ip_addresses :: LinodeIPAddresses,
                       label :: String,
                       services :: [Service],
                       state :: String,
                       total_transfer :: Int,
                       updated :: String } deriving (Show, GHC.Generics.Generic)

instance Data.Aeson.FromJSON Linode
instance Data.Aeson.ToJSON Linode

data Linodes = Linodes { total_pages :: Int,
                         total_results :: Int,
                         page :: Int,
                         linodes :: [Linode] } deriving (Show, GHC.Generics.Generic)

instance Data.Aeson.FromJSON Linodes
instance Data.Aeson.ToJSON Linodes

getLinodes :: IO (Data.Maybe.Maybe Linodes)
getLinodes = getData "/linodes"

getLinode :: String -> IO (Data.Maybe.Maybe Linode)
getLinode id = getSingle "/linodes" id

data Service = Service { id :: String,
                         storage :: Int,
                         hourly_price :: Int,
                         label :: String,
                         mbits_out :: Data.Maybe.Maybe Int,
                         monthly_price :: Int,
                         ram :: Int,
                         service_type :: String,
                         transfer :: Int,
                         vcpus :: Data.Maybe.Maybe Int } deriving (Show, GHC.Generics.Generic)

instance Data.Aeson.FromJSON Service where
  parseJSON (Data.Aeson.Object v) = do
    id :: String <- v .: "id" >>= Data.Aeson.parseJSON
    storage :: Int <- v .: "storage" >>= Data.Aeson.parseJSON
    hourly_price :: Int <- v.: "hourly_price" >>= Data.Aeson.parseJSON
    label :: String <- v .: "label" >>= Data.Aeson.parseJSON
    mbits_out :: Data.Maybe.Maybe Int <- optional $ v .: "mbits_out" >>= Data.Aeson.parseJSON
    monthly_price :: Int <- v .: "monthly_price" >>= Data.Aeson.parseJSON
    ram :: Int <- v .: "ram" >>= Data.Aeson.parseJSON
    service_type :: String <- v.: "service_type" >>= Data.Aeson.parseJSON
    transfer :: Int <- v .: "transfer" >>= Data.Aeson.parseJSON
    vcpus :: Data.Maybe.Maybe Int <- optional $ v .: "vcpus" >>= Data.Aeson.parseJSON
    return $ Service { id=id,
                       storage=storage,
                       hourly_price=hourly_price,
                       label=label,
                       mbits_out=mbits_out,
                       monthly_price=monthly_price,
                       ram=ram,
                       service_type=service_type,
                       transfer=transfer,
                       vcpus=vcpus }
instance Data.Aeson.ToJSON Service

data Services = Services { total_pages :: Int,
                           total_results :: Int,
                           page :: Int,
                           services :: [Service] } deriving (Show, GHC.Generics.Generic)

instance Data.Aeson.FromJSON Services
instance Data.Aeson.ToJSON Services

getServices :: IO (Data.Maybe.Maybe Services)
getServices = getData "/services"

getService :: String -> IO (Data.Maybe.Maybe Service)
getService id = getSingle "/services" id

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
                         display_group :: String } deriving (Show, GHC.Generics.Generic)

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

getDNSZone :: String -> IO (Data.Maybe.Maybe DNSZone)
getDNSZone id = getSingle "/dnszones" id

data Kernel = Kernel { id :: String,
                       created :: String,
                       deprecated :: Bool,
                       xen :: Bool,
                       kvm :: Bool,
                       label :: String,
                       version :: String,
                       x64 :: Bool } deriving (Show, GHC.Generics.Generic)

instance Data.Aeson.FromJSON Kernel
instance Data.Aeson.ToJSON Kernel

data Kernels = Kernels { total_pages :: Int,
                         total_results :: Int,
                         page :: Int,
                         kernels :: [Kernel] } deriving (Show, GHC.Generics.Generic)

instance Data.Aeson.FromJSON Kernels
instance Data.Aeson.ToJSON Kernels

getKernels :: IO (Data.Maybe.Maybe Kernels)
getKernels = getData "/kernels"

getKernel :: String -> IO (Data.Maybe.Maybe Kernel)
getKernel id = getSingle "/kernels" id

main = do
  linodes <- getLinodes
  _ <- case linodes of
    Just v -> print $ show v
    Nothing -> print "Error"
  linode <- getLinode "sdfasdf"
  case linode of
    Just v -> print $ show v
    Nothing -> print "Error"
