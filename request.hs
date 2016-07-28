{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Request where

import Data.Aeson
import qualified GHC.Generics (Generic)
import qualified Data.Maybe (Maybe)

data Linode = Linode { datacenter :: String,
                       service :: String } deriving (Show, GHC.Generics.Generic)

instance Data.Aeson.FromJSON Linode
instance Data.Aeson.ToJSON Linode
