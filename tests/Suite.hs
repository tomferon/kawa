import Control.Monad (unless)

import System.Exit (exitFailure)

import Hedgehog

import Database.Kawa.StoreTests

main :: IO ()
main = do
  succeeded <- checkParallel $ Group "Unit tests" $ storeProps
  unless succeeded exitFailure
