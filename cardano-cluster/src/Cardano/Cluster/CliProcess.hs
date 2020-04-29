{-# LANGUAGE OverloadedStrings #-}

module Cardano.Cluster.CliProcess
  ( cliSetup
  ) where

import           Cardano.Cluster.Environment

import           Cardano.Prelude

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (left)

import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS

import           Prelude (String)

import           System.Exit (ExitCode (..))
import           System.FilePath ((</>), takeFileName)
import qualified System.Process as Process


cliSetup :: ClusterEnv -> FilePath -> ExceptT ClusterError IO ()
cliSetup env rootDir = do
  cliCreateGeneis env rootDir
  cliGenerateKesKeys env rootDir
  cliGenerateVrfKeys env rootDir
  cliIssueOpCert env rootDir


cliCreateGeneis :: ClusterEnv -> FilePath -> ExceptT ClusterError IO ()
cliCreateGeneis env rootDir =
  void $ readProcess (ceCliProgram env)
          [ "shelley", "genesis", "create-genesis"
          , "--genesis-dir", rootDir
          , "--genesis-delegates", show (ceNodeCount env)
          , "--supply", "10000000000"
          ]

cliGenerateKesKeys :: ClusterEnv -> FilePath -> ExceptT ClusterError IO ()
cliGenerateKesKeys env rootDir =
  void $ readProcess (ceCliProgram env)
          [ "shelley", "node", "key-gen-KES"
          , "--verification-key-file", (rootDir </> "node-kes.vkey")
          , "--signing-key-file", (rootDir </> "node-kes.skey")
          , "--kes-duration", show (ceKesDuration env)
          ]

cliGenerateVrfKeys :: ClusterEnv -> FilePath -> ExceptT ClusterError IO ()
cliGenerateVrfKeys env rootDir =
  void $ readProcess (ceCliProgram env)
          [ "shelley", "node", "key-gen-VRF"
          , "--verification-key-file", (rootDir </> "node-vrf.vkey")
          , "--signing-key-file", (rootDir </> "node-vrf.skey")
          ]

cliIssueOpCert :: ClusterEnv -> FilePath -> ExceptT ClusterError IO ()
cliIssueOpCert env rootDir =
  void $ readProcess (ceCliProgram env)
          [ "shelley", "node", "issue-op-cert"
          , "--hot-kes-verification-key-file", (rootDir </> "node-kes.vkey")
          , "--cold-signing-key-file", (rootDir </> "delegate-keys" </> "delegate1.skey")
          , "--operational-certificate-issue-counter", (rootDir </> "delegate-keys" </> "delegate-opcert1.counter")
          , "--kes-period", "0"
          , "--out-file", (rootDir </> "op-cert-output")
          ]

-- | Like 'System.Process.readProcess' but captures 'stderr' and appends it to 'stdout'.
readProcess
    :: FilePath                         -- ^ Filename of the executable (see 'RawCommand' for details)
    -> [String]                         -- ^ any arguments
    -> ExceptT ClusterError IO ByteString   -- ^ output
readProcess cmd args = do
  (code, out, err) <- liftIO $ Process.readProcessWithExitCode cmd args ""
  case code of
    ExitSuccess ->
      pure $ BS.pack (err ++ out)
    ExitFailure _ ->
      left $ CliProcessError (takeFileName cmd) args (BS.pack out) (BS.pack err)
