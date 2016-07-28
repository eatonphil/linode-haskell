{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Api (getLinode, getLinodes)

import qualified Data.Maybe (Maybe (Just, Nothing) )

main = do
  linodes <- Api.getLinodes
  _ <- case linodes of
    Just v -> print $ show v
    Nothing -> print "Error"
  linode <- Api.getLinode "sdfasdf"
  case linode of
    Just v -> print $ show v
    Nothing -> print "Error"
