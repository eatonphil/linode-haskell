{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Request where

import qualified Response as Rsp (Filesystem, RunLevel, VirtMode)

import Data.Aeson
import qualified GHC.Generics (Generic)
import qualified Data.Maybe as M (Maybe)
import qualified Data.HashMap.Strict as H (HashMap)

data Linode = Linode { datacenter :: String,
                       service :: String,
                       source :: M.Maybe String,
                       root_pass :: M.Maybe String,
                       label :: M.Maybe String,
                       group :: M.Maybe String,
                       stackscript :: M.Maybe String,
                       stackscript_udf_responses :: M.Maybe (H.HashMap String String) } deriving (Eq, Show, GHC.Generics.Generic)

instance Data.Aeson.FromJSON Linode
instance Data.Aeson.ToJSON Linode

data Disk = Disk { size :: Int,
                   distribution :: M.Maybe String,
                   root_pass :: M.Maybe String,
                   root_ssh_key :: M.Maybe String,
                   label :: String,
                   filesystem :: Rsp.Filesystem,
                   read_only :: M.Maybe Bool,
                   stackscript :: M.Maybe String,
                   stackscript_udf_responses :: M.Maybe (H.HashMap String String) } deriving (Eq, Show, GHC.Generics.Generic)

instance Data.Aeson.FromJSON Disk
instance Data.Aeson.ToJSON Disk

data Config = Config { kernel :: String,
                       label :: String,
                       disks :: [String],
                       comments :: M.Maybe String,
                       ram_limit :: M.Maybe Int,
                       root_device_ro :: M.Maybe Bool,
                       devtmpfs_automount :: M.Maybe Bool,
                       run_level :: Rsp.RunLevel,
                       virt_mode :: Rsp.VirtMode } deriving (Eq, Show, GHC.Generics.Generic)

instance Data.Aeson.FromJSON Config
instance Data.Aeson.ToJSON Config
