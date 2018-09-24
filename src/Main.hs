module Main where

import           Network.Wai
import           Network.Wai.Logger
import           Network.Wai.Handler.Warp
import           Server
import           Effects
import           Sqlite
import           Models
import           Control.Monad.Reader

main :: IO ()
main = do
  let appSettings = Settings $ Sqlite "budget.db"
  runReaderT (runSqlite createDatabase) appSettings
  withStdoutLogger $ \appLogger -> do
    let settings = setPort 8081 $ setLogger appLogger defaultSettings
    runSettings settings (app appSettings)
