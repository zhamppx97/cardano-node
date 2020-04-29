{-# LANGUAGE MultiWayIf #-}
module Cardano.Cluster.Find
  ( findExecutable
  , setExecutableEnvVar
  ) where


import           Cardano.Cluster.Environment hiding (lookupEnv)
import           Cardano.Prelude

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (left)

import           Data.Bits ((.|.))
import qualified Data.Text as Text

import           Prelude (String)

import           System.Directory (canonicalizePath, getCurrentDirectory, listDirectory)
import           System.Environment (setEnv, lookupEnv)
import           System.FilePath ((</>), takeFileName)
import           System.Posix.Files

-- | findExecutable target startDir : Starting in the specified directory, find the executable
-- with name 'target' and return the full filepath.
findExecutable :: String -> FilePath -> ExceptT FindError IO FilePath
findExecutable target startDir = do
    xs <- liftIO $ filterM match =<< listDirectoryRecursive startDir
    case  xs of
      [] -> left $ FindNotFound (Text.pack target) startDir
      [y] -> liftIO $ canonicalizePath y
      _ -> left $ FindMultiple (Text.pack target) startDir
 where
  match :: FilePath -> IO Bool
  match fp
    | takeFileName fp /= target = pure False
    | otherwise = do
        st <- getFileStatus fp
        pure $ isRegularFile st && (fileMode st .|. ownerExecuteMode /= 0)

-- | setExecutableEnvVar envName target : If the 'envName' var is not set, find an
-- executable with name 'target' as assign the full filepath of that executable to
-- 'envName'.
setExecutableEnvVar :: String -> FilePath -> ExceptT FindError IO FilePath
setExecutableEnvVar envName target = do
  mEnv <- liftIO $ lookupEnv envName
  case mEnv of
    Just fpath -> pure fpath
    Nothing -> do
      startDir <- liftIO $ do
                    cwd <- getCurrentDirectory
                    canonicalizePath cwd
      path <- findExecutable target startDir
      liftIO $ setEnv envName path
      pure path

-- -------------------------------------------------------------------------------------------------

listDirectoryRecursive :: FilePath -> IO [FilePath]
listDirectoryRecursive fpath = do
  xs <- fmap (fpath </>) <$> listDirectory fpath
  (files, dirs) <- foldM partitioner ([], []) xs
  rest <- concatMapM listDirectoryRecursive (dirs :: [FilePath])
  pure $ files ++ rest
 where
  partitioner :: ([FilePath], [FilePath]) -> FilePath -> IO ([FilePath], [FilePath])
  partitioner (files, dirs) fp = do
    st <- getFileStatus fp
    if
      | isRegularFile st -> pure (fp : files, dirs)
      | isDirectory st -> pure (files, fp : dirs)
      | otherwise -> pure (files, dirs)
