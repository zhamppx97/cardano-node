
module Cardano.Cluster.Environment
  ( ClusterEnv (..)
  , ClusterError (..)
  , FindError (..)
  , lookupEnv
  , renderClusterError
  ) where

import           Cardano.Prelude

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (left)

import qualified Data.List as List
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import           Prelude (String)

import qualified System.Environment as Env


data ClusterEnv = ClusterEnv
  { ceCliProgram :: !FilePath
  , ceNodeProgram :: !FilePath
  , ceNodeCount :: !Word
  , ceKesDuration :: !Word
  }

data ClusterError
  = CliProcessError !String ![String] !ByteString !ByteString
  | ClusterEnvVar !String
  | ClusterFindError !FindError


data FindError
  = FindNotFound !Text !FilePath
  | FindMultiple !Text !FilePath
  deriving (Eq, Show)


lookupEnv :: String -> ExceptT ClusterError IO String
lookupEnv name =
   maybe (left $ ClusterEnvVar name) pure =<< liftIO (Env.lookupEnv name)

renderClusterError :: ClusterError -> Text
renderClusterError ce =
  case ce of
    CliProcessError prog args out err ->
      Text.unlines
        [ "Command failed:"
        , Text.pack ("  " ++ List.intercalate " " (prog : args))
        , "stdout:"
        , Text.decodeUtf8 out
        , "stderr:"
        , Text.decodeUtf8 err
        ]

    ClusterEnvVar var ->
      mconcat [ "Missing environment var: ",  textShow var ]

    ClusterFindError fe ->
      renderFindError fe

renderFindError :: FindError -> Text
renderFindError fe =
  case fe of
    FindNotFound tgt fp ->
      mconcat [ "Not able to find target '", tgt, "' starting in '", Text.pack fp, "'." ]
    FindMultiple tgt fp ->
      mconcat [ "Found multiple targets named '", tgt, "' starting in '", Text.pack fp, "'." ]

textShow :: Show a => a -> Text
textShow = Text.pack . show
