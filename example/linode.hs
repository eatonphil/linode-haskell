{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Network.Linode.Api as Api (getLinode, getLinodes, addLinode, editLinode)
import qualified Network.Linode.Response as Rsp (Linode (..))
import qualified Network.Linode.Request as Req (Linode)

import qualified Data.Aeson (encode)
import qualified Data.Maybe (Maybe (Just, Nothing) )

main = do
  linodes <- Api.getLinodes
  case linodes of
    Just v -> print $ show v
    Nothing -> print "Error 1"
  linode <- Api.getLinode "2019576"
  _ <- print $ show linode
  case linode of
    Just l -> do
      _ <- print $ Data.Aeson.encode l
      let newL = l { Rsp.label="xcvzxcvc22" }
      _ <- print $ Data.Aeson.encode newL
      linode <- Api.editLinode newL
      case linode of
        Just l -> print $ show l
        Nothing -> print "Error 3"
    Nothing -> print "Error 2"
