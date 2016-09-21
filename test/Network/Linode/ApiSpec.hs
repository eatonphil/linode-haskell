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
  getService,
  getKernels,
  getKernel)
import qualified Network.Linode.Response as Rsp (
  Datacenters (..),
  Kernels (..),
  Kernel (..),
  Datacenter (..),
  Services (..),
  Service (..))

latestKernel :: Rsp.Kernel
latestKernel = Rsp.Kernel {
  kvm = True,
  description = Just "By selecting this kernel, you will always boot with the latest Linux kernel we provide.\n<br><br>\n2011-08-21 - 3.0.0-linode35\n<br>\n2011-09-01 - 3.0.4-linode36\n<br>\n2011-09-12 - 3.0.4-linode37\n<br>\n2011-09-22 - 3.0.4-linode38\n<br>\n2012-01-23 - 3.0.17-linode41\n<br>\n2012-01-30 - 3.0.18-linode43\n<br>\n2012-06-13 - 3.4.2-linode44\n<br>\n2012-08-24 - 3.5.2-linode45\n<br>\n2012-11-10 - 3.6.5-linode47\n<br>\n2013-02-27 - 3.7.10-linode49\n<br>\n2013-03-35 - 3.8.4-linode50\n<br>\n2013-05-14 - 3.9.2-x86-linode51\n<br>\n2013-05-20 - 3.9.3-x86-linode52\n<br>\n2013-10-29 - 3.11.6-x86-linode54\n<br>\n2013-12-30 - 3.12.6-x86-linode55\n<br>\n2014-02-04 - 3.12.9-x86-linode56\n<br>\n2014-03-26 - 3.13.7-x86-linode57\n<br>\n2014-04-30 - 3.14.1-x86-linode58\n<br>\n2014-05-13 - 3.14.4-x86-linode59\n<br>\n2014-06-02 - 3.14.5-x86-linode60\n<br>\n2014-06-02 - 3.14.5-x86-linode61\n<br>\n2014-07-02 - 3.15.3-x86-linode63\n<br>\n2014-07-08 - 3.15.4-x86-linode64\n<br>\n2014-11-03 - 3.17.1-x86-linode66\n<br>\n2014-11-10 - 3.16.5-x86-linode65\n<br>\n2015-06-12 - 4.0.5-x86-linode77\n<br> \n2015-06-25 - 4.1.0-x86-linode78\n<br>\n2015-08-24 - 4.1.5-x86-linode80\n",
  version = "4.1.5",
  x64 = False,
  id = "linode/latest",
  label = "Latest 32 bit (4.1.5-x86-linode80)",
  created = "2011-09-01T21:08:55",
  deprecated = False,
  xen = True
}

linode80Kernel32Bit :: Rsp.Kernel
linode80Kernel32Bit = Rsp.Kernel {
  kvm = True,
  description = Nothing,
  version = "4.1.5",
  x64 = False,
  id = "linode/4.1.5-x86-linode80",
  label = "4.1.5-x86-linode80",
  created = "2015-08-24T15:00:43",
  deprecated = False,
  xen = True
}

linode79Kernel32Bit :: Rsp.Kernel
linode79Kernel32Bit = Rsp.Kernel {
  kvm = True,
  description = Nothing,
  version = "4.1.5",
  x64 = False,
  id = "linode/4.1.5-x86-linode79",
  label = "4.1.5-x86-linode79",
  created = "2015-08-13T09:00:00",
  deprecated = False,
  xen = True
}

linode78Kernel32Bit :: Rsp.Kernel
linode78Kernel32Bit = Rsp.Kernel {
  kvm = True,
  description = Nothing,
  version = "4.1.0",
  x64 = False,
  id = "linode/4.1.0-x86-linode78",
  label = "4.1.0-x86-linode78",
  created = "2015-06-22T11:19:32",
  deprecated = False,
  xen = True
}

linode77Kernel32Bit :: Rsp.Kernel
linode77Kernel32Bit = Rsp.Kernel {
  kvm = True,
  description = Nothing,
  version = "4.0.5",
  x64 = False,
  id = "linode/4.0.5-x86-linode77",
  label = "4.0.5-x86-linode77",
  created = "2015-06-11T09:58:18",
  deprecated = False,
  xen = True
}


linode61Kernel64Bit :: Rsp.Kernel
linode61Kernel64Bit = Rsp.Kernel {
  kvm = True,
  description = Nothing,
  version = "4.1.5",
  x64 = True,
  id = "linode/4.1.5-x86_64-linode61",
  label = "4.1.5-x86_64-linode61",
  created = "2015-08-24T15:00:43",
  deprecated = False,
  xen = True
}

linode60Kernel64Bit :: Rsp.Kernel
linode60Kernel64Bit = Rsp.Kernel {
  kvm = True,
  description = Nothing,
  version = "4.1.5",
  x64 = True,
  id = "linode/4.1.5-x86_64-linode60",
  label = "4.1.5-x86_64-linode60 ",
  created = "2015-08-13T09:00:00",
  deprecated = False,
  xen = True
}

linode59Kernel64Bit :: Rsp.Kernel
linode59Kernel64Bit = Rsp.Kernel {
  kvm = True,
  description = Nothing,
  version = "4.1.0",
  x64 = True,
  id = "linode/4.1.0-x86_64-linode59",
  label = "4.1.0-x86_64-linode59 ",
  created = "2015-06-22T11:19:32",
  deprecated = False,
  xen = True
}

linode58Kernel64Bit :: Rsp.Kernel
linode58Kernel64Bit = Rsp.Kernel {
  kvm = True,
  description = Nothing,
  version = "4.0.5",
  x64 = True,
  id = "linode/4.0.5-x86_64-linode58",
  label = "4.0.5-x86_64-linode58",
  created = "2015-06-10T11:31:52",
  deprecated = False,
  xen = True
}

linode57Kernel64Bit :: Rsp.Kernel
linode57Kernel64Bit = Rsp.Kernel {
  kvm = True,
  description = Nothing,
  version = "4.0.4",
  x64 = True,
  id = "linode/4.0.4-x86_64-linode57",
  label = "4.0.4-x86_64-linode57",
  created = "2015-05-21T11:15:47",
  deprecated = False,
  xen = True
}

linode56Kernel64Bit :: Rsp.Kernel
linode56Kernel64Bit = Rsp.Kernel {
  kvm = True,
  description = Nothing,
  version = "4.0.2",
  x64 = True,
  id = "linode/4.0.2-x86_64-linode56",
  label = "4.0.2-x86_64-linode56",
  created = "2015-05-11T16:56:58",
  deprecated = True,
  xen = True
}

linode55Kernel64Bit :: Rsp.Kernel
linode55Kernel64Bit = Rsp.Kernel {
  kvm = True,
  description = Nothing,
  version = "4.0.1",
  x64 = True,
  id = "linode/latest_64",
  label = "4.0.1-x86_64-linode55",
  created = "2015-05-04T09:43:23",
  deprecated = False,
  xen = False
}

grub2Kernel :: Rsp.Kernel
grub2Kernel = Rsp.Kernel {
  kvm = True,
  description = Just "Allows you to boot custom kernels and distributions using GRUB 2",
  version = "2.0.0",
  x64 = True,
  id = "linode/grub2",
  label = "GRUB 2",
  created = "2015-04-29T11:32:30",
  deprecated = False,
  xen = False
}

directBootKernel :: Rsp.Kernel
directBootKernel = Rsp.Kernel {
  kvm = True,
  description = Just "Boots the disk directly",
  version = "",
  x64 = True,
  id = "linode/direct-disk",
  label = "Direct Disk",
  created = "2015-05-04T21:51:43",
  deprecated = False,
  xen = False
}

grubLegacyKernel :: Rsp.Kernel
grubLegacyKernel = Rsp.Kernel {
  kvm = True,
  description = Just "Allows you to boot custom kernels and distributions using GRUB Legacy",
  version = "2.0.0",
  x64 = True,
  id = "linode/grub-legacy",
  label = "GRUB (Legacy)",
  created = "2015-04-29T11:32:30",
  deprecated = True,
  xen = False
}

finnixKernel :: Rsp.Kernel
finnixKernel = Rsp.Kernel {
  kvm = True,
  description = Nothing,
  version = "4.1.2",
  x64 = True,
  id = "linode/4.1.2-finnix",
  label = "Recovery - Finnix (kernel)",
  created = "2006-03-29T00:00:00",
  deprecated = False,
  xen = True
}

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

  describe "getKernels" $ do
    it "should return all kernels" $ do
      let expectedKernels = Just Rsp.Kernels {
        page = 1,
        total_pages = 1,
        total_results = 16,
        kernels = [
          latestKernel,
          linode80Kernel32Bit,
          linode79Kernel32Bit,
          linode78Kernel32Bit,
          linode77Kernel32Bit,
          linode61Kernel64Bit,
          linode60Kernel64Bit,
          linode59Kernel64Bit,
          linode58Kernel64Bit,
          linode57Kernel64Bit,
          linode56Kernel64Bit,
          linode55Kernel64Bit,
          grub2Kernel,
          directBootKernel,
          grubLegacyKernel,
          finnixKernel
        ] }
      kernels <- Api.getKernels
      kernels `shouldBe` expectedKernels

  describe "getLatestKernel" $ do
    it "should return the latest kernel" $ do
      let expectedKernel = Just Rsp.Kernel {
        kvm = True,
        description = Just "By selecting this kernel, you will always boot with the latest Linux kernel we provide.\n<br><br>\n2011-08-21 - 3.0.0-linode35\n<br>\n2011-09-01 - 3.0.4-linode36\n<br>\n2011-09-12 - 3.0.4-linode37\n<br>\n2011-09-22 - 3.0.4-linode38\n<br>\n2012-01-23 - 3.0.17-linode41\n<br>\n2012-01-30 - 3.0.18-linode43\n<br>\n2012-06-13 - 3.4.2-linode44\n<br>\n2012-08-24 - 3.5.2-linode45\n<br>\n2012-11-10 - 3.6.5-linode47\n<br>\n2013-02-27 - 3.7.10-linode49\n<br>\n2013-03-35 - 3.8.4-linode50\n<br>\n2013-05-14 - 3.9.2-x86-linode51\n<br>\n2013-05-20 - 3.9.3-x86-linode52\n<br>\n2013-10-29 - 3.11.6-x86-linode54\n<br>\n2013-12-30 - 3.12.6-x86-linode55\n<br>\n2014-02-04 - 3.12.9-x86-linode56\n<br>\n2014-03-26 - 3.13.7-x86-linode57\n<br>\n2014-04-30 - 3.14.1-x86-linode58\n<br>\n2014-05-13 - 3.14.4-x86-linode59\n<br>\n2014-06-02 - 3.14.5-x86-linode60\n<br>\n2014-06-02 - 3.14.5-x86-linode61\n<br>\n2014-07-02 - 3.15.3-x86-linode63\n<br>\n2014-07-08 - 3.15.4-x86-linode64\n<br>\n2014-11-03 - 3.17.1-x86-linode66\n<br>\n2014-11-10 - 3.16.5-x86-linode65\n<br>\n2015-06-12 - 4.0.5-x86-linode77\n<br> \n2015-06-25 - 4.1.0-x86-linode78\n<br>\n2015-08-24 - 4.1.5-x86-linode80\n",
        version = "4.1.5",
        x64 = False,
        id = "linode/latest",
        label = "Latest 32 bit (4.1.5-x86-linode80)",
        created = "2011-09-01T21:08:55",
        deprecated = False,
        xen = True
      }
      kernel <- Api.getKernel "linode/latest"
      kernel `shouldBe` expectedKernel

  describe "get80Kernel" $ do
    it "should return kernel number 80" $ do
      let expectedKernel = Just Rsp.Kernel {
        kvm = True,
        description = Nothing,
        version = "4.1.5",
        x64 = False,
        id = "linode/4.1.5-x86-linode80",
        label = "4.1.5-x86-linode80",
        created = "2015-08-24T15:00:43",
        deprecated = False,
        xen = True
      }
      kernel <- Api.getKernel "linode/4.1.5-x86-linode80"
      kernel `shouldBe` expectedKernel

  describe "get79Kernel" $ do
    it "should return kernel number 79" $ do
      let expectedKernel = Just Rsp.Kernel {
        kvm = True,
        description = Nothing,
        version = "4.1.5",
        x64 = False,
        id = "linode/4.1.5-x86-linode79",
        label = "4.1.5-x86-linode79",
        created = "2015-08-13T09:00:00",
        deprecated = False,
        xen = True
      }
      kernel <- Api.getKernel "linode/4.1.5-x86-linode79"
      kernel `shouldBe` expectedKernel

  describe "get78Kernel" $ do
    it "should return kernel number 78" $ do
      let expectedKernel = Just Rsp.Kernel {
        kvm = True,
        description = Nothing,
        version = "4.1.0",
        x64 = False,
        id = "linode/4.1.0-x86-linode78",
        label = "4.1.0-x86-linode78",
        created = "2015-06-22T11:19:32",
        deprecated = False,
        xen = True
      }
      kernel <- Api.getKernel "linode/4.1.0-x86-linode78"
      kernel `shouldBe` expectedKernel

  describe "get77Kernel" $ do
    it "should return kernel number 77" $ do
      let expectedKernel = Just Rsp.Kernel {
        kvm = True,
        description = Nothing,
        version = "4.0.5",
        x64 = False,
        id = "linode/4.0.5-x86-linode77",
        label = "4.0.5-x86-linode77",
        created = "2015-06-11T09:58:18",
        deprecated = False,
        xen = True
      }
      kernel <- Api.getKernel "linode/4.0.5-x86-linode77"
      kernel `shouldBe` expectedKernel


  describe "get61Kernel" $ do
    it "should return kernel number 61" $ do
      let expectedKernel = Just Rsp.Kernel {
        kvm = True,
        description = Nothing,
        version = "4.1.5",
        x64 = True,
        id = "linode/4.1.5-x86_64-linode61",
        label = "4.1.5-x86_64-linode61",
        created = "2015-08-24T15:00:43",
        deprecated = False,
        xen = True
      }
      kernel <- Api.getKernel "linode/4.1.5-x86_64-linode61"
      kernel `shouldBe` expectedKernel

  describe "get60Kernel" $ do
    it "should return kernel number 60" $ do
      let expectedKernel = Just Rsp.Kernel {
        kvm = True,
        description = Nothing,
        version = "4.1.5",
        x64 = True,
        id = "linode/4.1.5-x86_64-linode60",
        label = "4.1.5-x86_64-linode60 ",
        created = "2015-08-13T09:00:00",
        deprecated = False,
        xen = True
      }
      kernel <- Api.getKernel "linode/4.1.5-x86_64-linode60"
      kernel `shouldBe` expectedKernel

  describe "get59Kernel" $ do
    it "should return kernel number 59" $ do
      let expectedKernel = Just Rsp.Kernel {
        kvm = True,
        description = Nothing,
        version = "4.1.0",
        x64 = True,
        id = "linode/4.1.0-x86_64-linode59",
        label = "4.1.0-x86_64-linode59 ",
        created = "2015-06-22T11:19:32",
        deprecated = False,
        xen = True
      }
      kernel <- Api.getKernel "linode/4.1.0-x86_64-linode59"
      kernel `shouldBe` expectedKernel

  describe "get58Kernel" $ do
    it "should return kernel number 59" $ do
      let expectedKernel = Just Rsp.Kernel {
        kvm = True,
        description = Nothing,
        version = "4.0.5",
        x64 = True,
        id = "linode/4.0.5-x86_64-linode58",
        label = "4.0.5-x86_64-linode58",
        created = "2015-06-10T11:31:52",
        deprecated = False,
        xen = True
      }
      kernel <- Api.getKernel "linode/4.0.5-x86_64-linode58"
      kernel `shouldBe` expectedKernel

  describe "get57Kernel" $ do
    it "should return kernel number 57" $ do
      let expectedKernel = Just Rsp.Kernel {
        kvm = True,
        description = Nothing,
        version = "4.0.4",
        x64 = True,
        id = "linode/4.0.4-x86_64-linode57",
        label = "4.0.4-x86_64-linode57",
        created = "2015-05-21T11:15:47",
        deprecated = False,
        xen = True
      }
      kernel <- Api.getKernel "linode/4.0.4-x86_64-linode57"
      kernel `shouldBe` expectedKernel

  describe "get56Kernel" $ do
    it "should return kernel number 56" $ do
      let expectedKernel = Just Rsp.Kernel {
        kvm = True,
        description = Nothing,
        version = "4.0.2",
        x64 = True,
        id = "linode/4.0.2-x86_64-linode56",
        label = "4.0.2-x86_64-linode56",
        created = "2015-05-11T16:56:58",
        deprecated = True,
        xen = True
      }
      kernel <- Api.getKernel "linode/4.0.2-x86_64-linode56"
      kernel `shouldBe` expectedKernel

  describe "get55Kernel" $ do
    it "should return kernel number 55" $ do
      let expectedKernel = Just Rsp.Kernel {
        kvm = True,
        description = Nothing,
        version = "4.0.1",
        x64 = True,
        id = "linode/latest_64",
        label = "4.0.1-x86_64-linode55",
        created = "2015-05-04T09:43:23",
        deprecated = False,
        xen = False
      }
      kernel <- Api.getKernel "linode/latest_64"
      kernel `shouldBe` expectedKernel

  describe "getGrub2Kernel" $ do
    it "should return the grub2 kernel" $ do
      let expectedKernel = Just Rsp.Kernel {
        kvm = True,
        description = Just "Allows you to boot custom kernels and distributions using GRUB 2",
        version = "2.0.0",
        x64 = True,
        id = "linode/grub2",
        label = "GRUB 2",
        created = "2015-04-29T11:32:30",
        deprecated = False,
        xen = False
      }
      kernel <- Api.getKernel "linode/grub2"
      kernel `shouldBe` expectedKernel

  describe "getDirectBootKernel" $ do
    it "should return the direct boot 'kernel'" $ do
      let expectedKernel = Just Rsp.Kernel {
        kvm = True,
        description = Just "Boots the disk directly",
        version = "",
        x64 = True,
        id = "linode/direct-disk",
        label = "Direct Disk",
        created = "2015-05-04T21:51:43",
        deprecated = False,
        xen = False
      }
      kernel <- Api.getKernel "linode/direct-disk"
      kernel `shouldBe` expectedKernel

  describe "getGrubLegacyKernel" $ do
    it "should return the grub legacy" $ do
      let expectedKernel = Just Rsp.Kernel {
        kvm = True,
        description = Just "Allows you to boot custom kernels and distributions using GRUB Legacy",
        version = "2.0.0",
        x64 = True,
        id = "linode/grub-legacy",
        label = "GRUB (Legacy)",
        created = "2015-04-29T11:32:30",
        deprecated = True,
        xen = False
      }
      kernel <- Api.getKernel "linode/grub-legacy"
      kernel `shouldBe` expectedKernel

  describe "getFinnixKernel" $ do
    it "should return the finnix kernel" $ do
      let expectedKernel = Just Rsp.Kernel {
        kvm = True,
        description = Nothing,
        version = "4.1.2",
        x64 = True,
        id = "linode/4.1.2-finnix",
        label = "Recovery - Finnix (kernel)",
        created = "2006-03-29T00:00:00",
        deprecated = False,
        xen = True
      }
      kernel <- Api.getKernel "linode/4.1.2-finnix"
      kernel `shouldBe` expectedKernel

  describe "getServices" $ do
    it "should return all services" $ do
      let expectedServices = Just Rsp.Services {
            page = 1,
            total_pages = 1,
            total_results = 1,
            services = [ Rsp.Service {
                           label = "Linode 2048",
                           ram = Just 2048,
                           hourly_price = 1,
                           id = "linode2048.5",
                           transfer = Just 2000,
                           monthly_price = 1000,
                           storage = Just 24576,
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
            storage = Just 24576,
            mbits_out = Just 125,
            vcpus = Just 1 }
      service <- Api.getService "linode2048.5"
      service `shouldBe` expectedService
