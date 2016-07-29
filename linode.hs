{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Api (getLinode, getLinodes, addLinode, editLinode)
import qualified Response as Rsp (Linode (..))
import qualified Request as Req (Linode)

import qualified Data.Aeson (encode)
import qualified Data.Maybe (Maybe (Just, Nothing) )

main = do
  linodes <- Api.getLinodes
  _ <- case linodes of
    Just v -> print $ show v
    Nothing -> print "Error 1"
  linode <- Api.getLinode "sdfasdf"
  _ <- print $ show linode
  case linode of
    Just l -> do
      _ <- print $ Data.Aeson.encode l
      let newL = l { Rsp.label="sdfasdf1" }
      _ <- print $ Data.Aeson.encode newL
      linode <- Api.editLinode newL
      case linode of
        Just l -> print $ show l
        Nothing -> print "Error 3"
    Nothing -> print "Error 2"
