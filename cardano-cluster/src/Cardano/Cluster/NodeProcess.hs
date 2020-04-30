{-# LANGUAGE OverloadedStrings #-}

module Cardano.Cluster.NodeProcess
  ( runNodes
  ) where

import           Cardano.Cluster.Environment

import           Cardano.Prelude

import           Control.Concurrent.Async.Lifted

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (left)

import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS

import           Prelude (String)


runNodes :: ClusterEnv -> ExceptT ClusterError IO ()
runNodes _ = undefined -- env = undefined env count



runSingleNode :: ClusterEnv -> Word ->
