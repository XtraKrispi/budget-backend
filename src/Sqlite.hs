{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Sqlite where

import Control.Monad.Free
import Database.SQLite.Simple
import Text.RawString.QQ
import Control.Monad.IO.Class
import Models
import Effects

dbFile = "budget.db"

runDb :: (MonadIO m) => DbAccess a -> m a
runDb = 
  liftIO . interpret dbFile


interpret :: String -> DbAccess r -> IO r
interpret dbFile program = do 
  conn <- open dbFile
  withTransaction conn $
    sqliteInterpreterHelper conn program
  where
    sqliteInterpreterHelper :: Connection -> DbAccess r -> IO r
    sqliteInterpreterHelper conn (Free (CreateDatabase g)) = do
      execute_ conn [r| CREATE TABLE IF NOT EXISTS definitions 
                        ( id INTEGER PRIMARY KEY
                        , description TEXT
                        , amount REAL
                        , startDate TEXT
                        , frequency TEXT
                        )
                    |]
      sqliteInterpreterHelper conn g

    sqliteInterpreterHelper conn (Free (GetBudgetItemDefinitions g)) = do
      results <- query_ conn [r|  SELECT id
                                       , description
                                       , amount
                                       , startDate
                                       , frequency
                                  FROM definitions                                 
                              |]
      sqliteInterpreterHelper conn (g results)

    sqliteInterpreterHelper conn (Free (GetBudgetItemInstances s e g)) =
      sqliteInterpreterHelper conn (g [])
      
    sqliteInterpreterHelper _ (Pure r) = return r