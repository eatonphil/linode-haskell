{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Request where

import qualified Response as Rsp (Filesystem, RunLevel, VirtMode)

import Data.Aeson
import qualified GHC.Generics (Generic)
import qualified Data.Maybe (Maybe)
import qualified Data.HashMap.Strict as H (HashMap)

data Linode = Linode { datacenter :: String,
                       service :: String,
                       source :: Data.Maybe.Maybe String,
                       root_pass :: Data.Maybe.Maybe String,
                       label :: Data.Maybe.Maybe String,
                       group :: Data.Maybe.Maybe String,
                       stackscript :: Data.Maybe.Maybe String,
                       stackscript_udf_responses :: Data.Maybe.Maybe (H.HashMap String String) } deriving (Eq, Show, GHC.Generics.Generic)

instance Data.Aeson.FromJSON Linode
instance Data.Aeson.ToJSON Linode

data Disk = Disk { size :: Int,
                   distribution :: Data.Maybe.Maybe String,
                   root_pass :: Data.Maybe.Maybe String,
                   root_ssh_key :: Data.Maybe.Maybe String,
                   label :: String,
                   filesystem :: Rsp.Filesystem,
                   read_only :: Data.Maybe.Maybe Bool,
                   stackscript :: Data.Maybe.Maybe String,
                   stackscript_udf_responses :: Data.Maybe.Maybe (H.HashMap String String) } deriving (Eq, Show, GHC.Generics.Generic)

instance Data.Aeson.FromJSON Disk
instance Data.Aeson.ToJSON Disk

data Config = Config { kernel :: String,
                       label :: String,
                       disks :: [String],
                       comments :: Data.Maybe.Maybe String,
                       ram_limit :: Data.Maybe.Maybe Int,
                       root_device_ro :: Data.Maybe.Maybe Bool,
                       devtmpfs_automount :: Data.Maybe.Maybe Bool,
                       run_level :: Rsp.RunLevel,
                       virt_mode :: Rsp.VirtMode } deriving (Eq, Show, GHC.Generics.Generic)

instance Data.Aeson.FromJSON Config
instance Data.Aeson.ToJSON Config
