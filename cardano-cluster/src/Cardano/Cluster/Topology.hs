{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}

module Cardano.Cluster.Topology
  ( mkTopology
  , writeTopology
  ) where

import           Cardano.Prelude hiding (toS)
import           Cardano.Config.Topology (NetworkTopology (..), RemoteAddress (..))

import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as LBS


-- | mkTopology count self : Make a NetworkTopology object with 'count' endpoints but
-- without 'self'.
mkTopology :: Int -> Int -> NetworkTopology
mkTopology count self =
    RealNodeTopology $ mapMaybe generate [1 .. count]
  where
    generate :: Int -> Maybe RemoteAddress
    generate indx
      | indx == self = Nothing
      | otherwise =
          Just $ RemoteAddress
                  { raAddress = "127.0.0.1"
                  , raPort = fromIntegral (3300 + indx)
                  , raValency = 1
                  }

-- | Write a NetworkTopology object as JSON to the specified file.
writeTopology :: FilePath -> NetworkTopology -> IO ()
writeTopology fpath top =
  LBS.writeFile fpath $ encodePretty top
