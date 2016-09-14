{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Linode.ApiSpec (main, spec) where

import Test.Hspec

import qualified Network.Linode.Api as Api (
  getDatacenters,
  getDatacenter,
  getServices,
  getService)
import qualified Network.Linode.Response as Rsp (
  Datacenters (..),
  Datacenter (..),
  Services (..),
  Service (..))

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "getDatacenters" $ do
    it "should return all datacenters" $ do
      let expectedDatacenters = Just Rsp.Datacenters {
            page = 1,
            total_pages = 1,
            total_results = 1,
            datacenters = [ Rsp.Datacenter {
                              label = "Newark, NJ",
                              country = "us",
                              id = "newark" } ] }
      datacenters <- Api.getDatacenters
      datacenters `shouldBe` expectedDatacenters

  describe "getDatacenter" $ do
    it "should return newark datacenter" $ do
      let expectedDatacenter = Just Rsp.Datacenter {
            label = "Newark, NJ",
            country = "us",
            id = "newark" }
      datacenter <- Api.getDatacenter "newark"
      datacenter `shouldBe` expectedDatacenter

  describe "getServices" $ do
    it "should return all services" $ do
      let expectedServices = Just Rsp.Services {
            page = 1,
            total_pages = 1,
            total_results = 2,
            services = [ Rsp.Service {
                           label = "NodeBalancer",
                           ram = Nothing,
                           hourly_price = 3,
                           id = "balancer",
                           transfer = Nothing,
                           monthly_price = 2000,
                           service_type = "nodebalancer",
                           storage = Nothing,
                           mbits_out = Nothing,
                           vcpus = Nothing },
                         Rsp.Service {
                           label = "Linode 2048",
                           ram = Just 2048,
                           hourly_price = 1,
                           id = "linode2048.5",
                           transfer = Just 2000,
                           monthly_price = 1000,
                           service_type = "linode",
                           storage = Just 24,
                           mbits_out = Just 125,
                           vcpus = Just 1 } ] }
      services <- Api.getServices
      services `shouldBe` expectedServices

    it "should return the Linode 2048 service" $ do
      let expectedService = Just Rsp.Service {
            label = "Linode 2048",
            ram = Just 2048,
            hourly_price = 1,
            id = "linode2048.5",
            transfer = Just 2000,
            monthly_price = 1000,
            service_type = "linode",
            storage = Just 24,
            mbits_out = Just 125,
            vcpus = Just 1 }
      service <- Api.getService "linode2048.5"
      service `shouldBe` expectedService
