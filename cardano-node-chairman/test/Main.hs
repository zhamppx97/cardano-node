module Main
  ( main
  ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Function
import           Hedgehog
import           Hedgehog.Extras.Test.Base
import           System.IO (IO)

import qualified Control.Concurrent as IO
import qualified System.IO as IO


main :: IO ()
main = do
  void . check . propertyOnce $ do
    void . register $ IO.appendFile "log.txt" "Cleanup\n"
    void . liftResourceT . resourceForkIO $ do
      liftIO $ IO.appendFile "log.txt" "Forked\n"
      void . liftIO $ IO.threadDelay 5000000
    liftIO $ IO.threadDelay 1000000
    liftIO $ IO.appendFile "log.txt" "Done\n"
    liftIO $ IO.threadDelay 1000000

  void . forever $ IO.threadDelay 100000000

  return ()
