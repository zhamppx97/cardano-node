
import           Cardano.Cluster
import           Cardano.Prelude

import           Control.Monad.Trans.Except.Exit (orDie)
import           Control.Monad.Trans.Except.Extra (firstExceptT)


main :: IO ()
main =
  orDie renderClusterError $ do
    cliProg <- firstExceptT ClusterFindError $ setExecutableEnvVar "CARDANO_CLI" "cardano-cli"
    nodeProg <- firstExceptT ClusterFindError $ setExecutableEnvVar "CARDANO_NODE" "cardano-node"

    cliSetup (ClusterEnv cliProg nodeProg 1 100) "shelley"

