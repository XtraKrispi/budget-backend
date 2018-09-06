module Main where

import           Network.Wai
import           Network.Wai.Handler.Warp
import           Server
import           Effects
import           Sqlite

main :: IO ()
main = do
  runDb createDatabase
  run 8081 app
