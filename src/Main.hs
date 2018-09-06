module Main where

import           Network.Wai
import           Network.Wai.Logger
import           Network.Wai.Handler.Warp
import           Server
import           Effects
import           Sqlite

main :: IO ()
main = do
  runDb createDatabase
  withStdoutLogger $ \appLogger -> do
    let settings = setPort 8081 $ setLogger appLogger defaultSettings
    runSettings settings app
