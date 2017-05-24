{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Network.Linode.Response.Regions (Region, Regions)
import qualified Network.Linode.Api as Api (getRegions, getDistributions)

import qualified Data.Maybe (Maybe (Just, Nothing))

main = do
  regions <- Api.getRegions
  distributions <- Api.getDistributions
  case regions of
    Just v -> print $ show v
    Nothing -> print "Error 1"
  case distributions of
    Just v -> print $ show v
    Nothing -> print "Error 2"
