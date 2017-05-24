module Network.Linode.Response.Region where

import Data.Aeson


data Region = Region { regionId :: String,
                       regionCountry :: String,
                       regionLabel :: String } deriving (Eq, Show)

instance Data.Aeson.FromJSON Region where
  parseJSON (Data.Aeson.Object v) =
    Region <$> v .: "id"
           <*> v .: "country"
           <*> v .: "label"

instance Data.Aeson.ToJSON Region where
 toJSON (Region regionId regionCountry regionLabel) =
    Data.Aeson.object [ "id"                   .= regionId,
                        "created"              .= regionCountry,
                        "vendor"               .= regionLabel ]

data Regions = Regions { regionsTotalPages :: Int,
                         regionsTotalResults :: Int,
                         regionsPage :: Int,
                         regionsRegions :: [Region] } deriving (Eq, Show)

instance Data.Aeson.FromJSON Regions where
  parseJSON (Data.Aeson.Object v) =
    Regions <$> v .: "total_pages"
            <*> v .: "total_results"
            <*> v .: "page"
            <*> v .: "regions"
    
instance Data.Aeson.ToJSON Regions where
  toJSON (Regions regionsTotalPages regionsTotalResults regionsPage regionsRegions) =
    Data.Aeson.object [ "total_pages"   .= regionsTotalPages,
                        "total_results" .= regionsTotalResults,
                        "page"          .= regionsPage,
                        "regions"       .= regionsRegions ]
