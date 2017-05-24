module Network.Linode.Response.Distribution where

import Data.Aeson


data Distribution = Distribution { distributionId                 :: String,
                                   distributionCreated            :: String,
                                   distributionVendor             :: String,
                                   distributionRecommended        :: Bool,
                                   distributionMinimumStorageSize :: Int,
                                   distributionX64                :: Bool,
                                   distributionLabel              :: String } deriving (Eq, Show)

instance Data.Aeson.FromJSON Distribution where
  parseJSON (Data.Aeson.Object v) =
    Distribution <$> v .: "id"
                 <*> v .: "created"
                 <*> v .: "vendor"
                 <*> v .: "deprecated"
                 <*> v .: "minimum_storage_size"
                 <*> v .: "x64"
                 <*> v .: "label"

instance Data.Aeson.ToJSON Distribution where
 toJSON (Distribution distributionId distributionCreated distributionVendor distributionDeprecated distributionMinimumStorageSize distributionX64 distributionLabel) =
    Data.Aeson.object [ "id"                   .= distributionId,
                        "created"              .= distributionCreated,
                        "vendor"               .= distributionVendor,
                        "deprecated"          .= distributionDeprecated,
                        "minimum_storage_size" .= distributionMinimumStorageSize,
                        "x64"                  .= distributionX64,
                        "label"                .= distributionLabel ]

data Distributions = Distributions { distributionsTotalPages    :: Int,
                                     distributionsTotalResults  :: Int,
                                     distributionsPage          :: Int,
                                     distributionsDistributions :: [Distribution] } deriving (Eq, Show)

instance Data.Aeson.FromJSON Distributions where
  parseJSON (Data.Aeson.Object v) =
    Distributions <$> v .: "total_pages"
                  <*> v .: "total_results"
                  <*> v .: "page"
                  <*> v .: "distributions"
    
instance Data.Aeson.ToJSON Distributions where
  toJSON (Distributions distributionsTotalPages distributionsTotalResults distributionsPage distributionsDistributions) =
    Data.Aeson.object [ "total_pages"   .= distributionsTotalPages,
                        "total_results" .= distributionsTotalResults,
                        "page"          .= distributionsPage,
                        "distributions" .= distributionsDistributions ]
