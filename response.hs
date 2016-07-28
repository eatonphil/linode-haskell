{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Response where

import Data.Aeson
import qualified GHC.Generics (Generic)
import qualified Data.Maybe (Maybe)

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


instance Data.Aeson.FromJSON Service
instance Data.Aeson.ToJSON Service

data Services = Services { total_pages :: Int,
                           total_results :: Int,
                           page :: Int,
                           services :: [Service] } deriving (Show, GHC.Generics.Generic)

instance Data.Aeson.FromJSON Services
instance Data.Aeson.ToJSON Services


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

instance Data.Aeson.FromJSON LinodeBackupsSchedule
instance Data.Aeson.ToJSON LinodeBackupsSchedule

data LinodeBackups = LinodeBackups { enabled :: Bool,
                                     schedule :: LinodeBackupsSchedule,
                                     last_backup :: Data.Maybe.Maybe String } deriving (Show, GHC.Generics.Generic)

instance Data.Aeson.FromJSON LinodeBackups
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
