module Main where

import Lib
import Network.Wai.Handler.Warp (run)
import Paths_test_covidapp (getDataDir)

main :: IO ()
main = do
  dataDir <- getDataDir
  putStrLn "http://localhost:8080/"
  run 8080 $ app dataDir
