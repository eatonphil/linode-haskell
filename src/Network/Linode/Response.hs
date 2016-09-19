{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Linode.Response where

import Data.Aeson
import qualified GHC.Generics (Generic)
import qualified Data.Maybe as M (Maybe)

data Datacenter = Datacenter { id :: String,
                               country :: String,
                               label :: String } deriving (Eq, Show, GHC.Generics.Generic)

instance Data.Aeson.FromJSON Datacenter
instance Data.Aeson.ToJSON Datacenter

data Datacenters = Datacenters { total_pages :: Int,
                                 total_results :: Int,
                                 page :: Int,
                                 datacenters :: [Datacenter] } deriving (Eq, Show, GHC.Generics.Generic)

instance Data.Aeson.FromJSON Datacenters
instance Data.Aeson.ToJSON Datacenters

data Distribution = Distribution { id :: String,
                                   created :: String,
                                   vendor :: String,
                                   recommended :: Bool,
                                   minimum_storage_size :: Int,
                                   x64 :: Bool,
                                   label :: String } deriving (Eq, Show, GHC.Generics.Generic)

instance Data.Aeson.FromJSON Distribution
instance Data.Aeson.ToJSON Distribution

data Distributions = Distributions { total_pages :: Int,
                                     total_results :: Int,
                                     page :: Int,
                                     distributions :: [Distributions] } deriving (Eq, Show, GHC.Generics.Generic)

instance Data.Aeson.FromJSON Distributions
instance Data.Aeson.ToJSON Distributions

data Service = Service { id :: String,
                         storage :: Maybe Int,
                         hourly_price :: Int,
                         label :: String,
                         mbits_out :: M.Maybe Int,
                         monthly_price :: Int,
                         ram :: M.Maybe Int,
                         service_type :: String,
                         transfer :: M.Maybe Int,
                         vcpus :: M.Maybe Int } deriving (Eq, Show, GHC.Generics.Generic)


instance Data.Aeson.FromJSON Service
instance Data.Aeson.ToJSON Service

data Services = Services { total_pages :: Int,
                           total_results :: Int,
                           page :: Int,
                           services :: [Service] } deriving (Eq, Show, GHC.Generics.Generic)

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
                         display_group :: String } deriving (Eq, Show, GHC.Generics.Generic)

instance Data.Aeson.FromJSON DNSZone
instance Data.Aeson.ToJSON DNSZone

data DNSZones = DNSZones { total_pages :: Int,
                           total_results :: Int,
                           page :: Int,
                           dnszones :: [DNSZone] } deriving (Eq, Show, GHC.Generics.Generic)

instance Data.Aeson.FromJSON DNSZones
instance Data.Aeson.ToJSON DNSZones

data Kernel = Kernel { id :: String,
                       created :: String,
                       deprecated :: Bool,
                       xen :: Bool,
                       kvm :: Bool,
                       description :: M.Maybe String,
                       label :: String,
                       version :: String,
                       x64 :: Bool } deriving (Eq, Show, GHC.Generics.Generic)

instance Data.Aeson.FromJSON Kernel
instance Data.Aeson.ToJSON Kernel

data Kernels = Kernels { total_pages :: Int,
                         total_results :: Int,
                         page :: Int,
                         kernels :: [Kernel] } deriving (Eq, Show, GHC.Generics.Generic)

instance Data.Aeson.FromJSON Kernels
instance Data.Aeson.ToJSON Kernels


data LinodeAlert = LinodeAlert { enabled :: Bool,
                                 threshold :: Int } deriving (Eq, Show, GHC.Generics.Generic)

instance Data.Aeson.FromJSON LinodeAlert
instance Data.Aeson.ToJSON LinodeAlert

data LinodeAlerts = LinodeAlerts { cpu :: LinodeAlert,
                                   io :: LinodeAlert,
                                   transfer_in :: LinodeAlert,
                                   transfer_out :: LinodeAlert,
                                   transfer_quota :: LinodeAlert } deriving (Eq, Show, GHC.Generics.Generic)

instance Data.Aeson.FromJSON LinodeAlerts
instance Data.Aeson.ToJSON LinodeAlerts

data LinodeBackupsSchedule = LinodeBackupsSchedule { day :: M.Maybe String,
                                                     window :: M.Maybe String } deriving (Eq, Show, GHC.Generics.Generic)

instance Data.Aeson.FromJSON LinodeBackupsSchedule
instance Data.Aeson.ToJSON LinodeBackupsSchedule


data LinodeBackups = LinodeBackups { enabled :: Bool,
                                     schedule :: LinodeBackupsSchedule,
                                     snapshot :: M.Maybe Backup,
                                     last_backup :: M.Maybe Backup } deriving (Eq, Show, GHC.Generics.Generic)

instance Data.Aeson.FromJSON LinodeBackups
instance Data.Aeson.ToJSON LinodeBackups

data LinodeIPAddressesPrivate = LinodeIPAddressesPrivate { ipv4 :: [String],
                                                           link_local :: String } deriving (Eq, Show, GHC.Generics.Generic)

instance Data.Aeson.FromJSON LinodeIPAddressesPrivate
instance Data.Aeson.ToJSON LinodeIPAddressesPrivate

data LinodeIPAddressesPublic = LinodeIPAddressesPublic { failover :: [String],
                                                         ipv4 :: [String],
                                                         ipv6 :: String } deriving (Eq, Show, GHC.Generics.Generic)

instance Data.Aeson.FromJSON LinodeIPAddressesPublic
instance Data.Aeson.ToJSON LinodeIPAddressesPublic

data LinodeIPAddresses = LinodeIPAddresses { private :: LinodeIPAddressesPrivate,
                                             public :: LinodeIPAddressesPublic } deriving (Eq, Show, GHC.Generics.Generic)

instance Data.Aeson.FromJSON LinodeIPAddresses
instance Data.Aeson.ToJSON LinodeIPAddresses

data Linode = Linode { id :: Int,
                       alerts :: LinodeAlerts,
                       backups :: LinodeBackups,
                       created :: String,
                       datacenter :: Datacenter,
                       distribution :: M.Maybe Distribution,
                       group :: String,
                       ips :: LinodeIPAddresses,
                       label :: String,
                       services :: [Service],
                       state :: String,
                       total_transfer :: Int,
                       updated :: String } deriving (Eq, Show, GHC.Generics.Generic)

instance Data.Aeson.FromJSON Linode
instance Data.Aeson.ToJSON Linode

data Linodes = Linodes { total_pages :: Int,
                         total_results :: Int,
                         page :: Int,
                         linodes :: [Linode] } deriving (Eq, Show, GHC.Generics.Generic)

instance Data.Aeson.FromJSON Linodes
instance Data.Aeson.ToJSON Linodes

-- Missing type field
data Backup = Backup { id :: Int,
                       label :: M.Maybe String,
                       status :: String,
                       datacenter :: Datacenter,
                       created :: String,
                       updated :: String,
                       finished :: String } deriving (Eq, Show, GHC.Generics.Generic)

instance Data.Aeson.FromJSON Backup
instance Data.Aeson.ToJSON Backup

data Backups = Backups { total_pages :: Int,
                         total_results :: Int,
                         page :: Int,
                         backups :: [Backup] } deriving (Eq, Show, GHC.Generics.Generic)

instance Data.Aeson.FromJSON Backups
instance Data.Aeson.ToJSON Backups

data Filesystem = Raw | Swap | Ext3 | Ext4 deriving (Eq, Show)

instance Data.Aeson.ToJSON Filesystem where
  toJSON o =
    let json :: String = case o of
          Raw -> "raw"
          Swap -> "swap"
          Ext3 -> "ext3"
          Ext4 -> "ext4" in
    Data.Aeson.object [ "filesystem" .= json ]

instance Data.Aeson.FromJSON Filesystem where
  parseJSON o =
    return $ case o of
      "raw" -> Raw
      "swap" -> Swap
      "ext3" -> Ext3
      "ext4" -> Ext4

data Disk = Disk { id :: String,
                   label :: String,
                   state :: String,
                   size :: Int,
                   filesystem :: Filesystem,
                   created :: String,
                   updated :: String } deriving (Eq, Show, GHC.Generics.Generic)

instance Data.Aeson.FromJSON Disk
instance Data.Aeson.ToJSON Disk

data Disks = Disks { total_pages :: Int,
                     total_results :: Int,
                     page :: Int,
                     backups :: [Disk] } deriving (Eq, Show, GHC.Generics.Generic)

instance Data.Aeson.FromJSON Disks
instance Data.Aeson.ToJSON Disks

data ConfigDisks = ConfigDisks { sda :: M.Maybe Disk,
                                 sdb :: M.Maybe Disk,
                                 sdc :: M.Maybe Disk,
                                 sdd :: M.Maybe Disk,
                                 sde :: M.Maybe Disk,
                                 sdf :: M.Maybe Disk,
                                 sdg :: M.Maybe Disk,
                                 sdh :: M.Maybe Disk } deriving (Eq, Show, GHC.Generics.Generic)

instance Data.Aeson.FromJSON ConfigDisks
instance Data.Aeson.ToJSON ConfigDisks

data ConfigHelpers = ConfigHelpers { disable_updatedb :: Bool,
                                     enable_distro_helper :: Bool,
                                     enable_modules_deb_helper :: Bool,
                                     enable_network_helper :: Bool } deriving (Eq, Show, GHC.Generics.Generic)

instance Data.Aeson.FromJSON ConfigHelpers
instance Data.Aeson.ToJSON ConfigHelpers

data Config = Config { id :: String,
                       comments :: String,
                       created :: String,
                       devtmpfs_automount :: Bool,
                       disks :: ConfigDisks,
                       helpers :: ConfigHelpers,
                       kernel :: Kernel,
                       label :: String,
                       ram_limit :: M.Maybe Int,
                       root_device :: String,
                       root_device_ro :: Bool,
                       run_level :: String,
                       updated :: String,
                       virt_mode :: String } deriving (Eq, Show, GHC.Generics.Generic)

instance Data.Aeson.FromJSON Config
instance Data.Aeson.ToJSON Config

data Configs = Configs { total_pages :: Int,
                         total_results :: Int,
                         page :: Int,
                         configs :: [Config] } deriving (Eq, Show, GHC.Generics.Generic)


instance Data.Aeson.FromJSON Configs
instance Data.Aeson.ToJSON Configs

-- Missing default field
data UDF = UDF { name :: String,
                 label :: String,
                 example :: String } deriving (Eq, Show, GHC.Generics.Generic)

instance Data.Aeson.FromJSON UDF
instance Data.Aeson.ToJSON UDF

data StackScript = StackScript { id :: String,
                                 customer_id :: String,
                                 user_id :: String,
                                 label :: String,
                                 description :: String,
                                 distributions :: [Distribution],
                                 deployments_total :: Int,
                                 deployments_active :: Int,
                                 is_public :: Bool,
                                 created :: String,
                                 updated :: String,
                                 rev_note :: String,
                                 script :: String,
                                 user_defined_fields :: [UDF] } deriving (Eq, Show, GHC.Generics.Generic)

instance Data.Aeson.FromJSON StackScript
instance Data.Aeson.ToJSON StackScript

data StackScripts = StackScripts { total_pages :: Int,
                                   total_results :: Int,
                                   page :: Int,
                                   stackscripts :: [StackScripts] } deriving (Eq, Show, GHC.Generics.Generic)

instance Data.Aeson.FromJSON StackScripts
instance Data.Aeson.ToJSON StackScripts

data RunLevel = Default | Single | BinBash deriving (Eq, Show)

instance Data.Aeson.ToJSON RunLevel where
  toJSON o =
    let json :: String = case o of
          Default -> "default"
          Single -> "single"
          BinBash -> "binbash" in
    Data.Aeson.object [ "run_level" .= json ]

instance Data.Aeson.FromJSON RunLevel where
  parseJSON o =
    return $ case o of
      "default" -> Default
      "single" -> Single
      "binbash" -> BinBash

data VirtMode = FullVirt | ParaVirt deriving (Eq, Show)

instance Data.Aeson.ToJSON VirtMode where
  toJSON o =
    let json :: String = case o of
          FullVirt -> "fullvirt"
          ParaVirt -> "paravirt" in
    Data.Aeson.object [ "virt_mode" .= json ]

instance Data.Aeson.FromJSON VirtMode where
  parseJSON o =
    return $ case o of
      "fullvirt" -> FullVirt
      "paravirt" -> ParaVirt
