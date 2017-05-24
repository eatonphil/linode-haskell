module Network.Linode.Response.Linode where

import Data.Aeson


data Linode = Linode { linodeId :: Int,
                       linodeAlerts :: LinodeAlerts,
                       linodeBackups :: LinodeBackups,
                       linodeCreated :: String,
                       linodeRegion :: Region.Region,
                       linodeDistribution :: Distribution.Distribution,
                       linodeGroup :: String,
                       linodeIPv4 :: [IPv4.IPv4],
                       linodeSlaac :: String,
                       linodeLabel :: String,
                       linodeType :: String,
                       linodeStatus :: String,
                       linodeTotalTransfer :: Int,
                       linodeUpdated :: String,
                       linodeHypervisor :: String } deriving (Eq, Show)

instance Data.Aeson.FromJSON Linode where
  parseJSON (Data.Aeson.Object v) =
    Linode <$> v .: "id"
           <*> v .: "country"
           <*> v .: "label"

instance Data.Aeson.ToJSON Linode where
 toJSON (Linode regionId regionCountry regionLabel) =
    Data.Aeson.object [ "id"                   .= regionId,
                        "created"              .= regionCountry,
                        "vendor"               .= regionLabel ]

data Linodes = Linodes { regionsTotalPages :: Int,
                         regionsTotalResults :: Int,
                         regionsPage :: Int,
                         regionsLinodes :: [Linode] } deriving (Eq, Show)

instance Data.Aeson.FromJSON Linodes where
  parseJSON (Data.Aeson.Object v) =
    Linodes <$> v .: "total_pages"
            <*> v .: "total_results"
            <*> v .: "page"
            <*> v .: "regions"
    
instance Data.Aeson.ToJSON Linodes where
  toJSON (Linodes regionsTotalPages regionsTotalResults regionsPage regionsLinodes) =
    Data.Aeson.object [ "total_pages"   .= regionsTotalPages,
                        "total_results" .= regionsTotalResults,
                        "page"          .= regionsPage,
                        "regions"       .= regionsLinodes ]
