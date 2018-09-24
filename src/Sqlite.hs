{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
module Sqlite where

import Control.Monad.Free
import Database.SQLite.Simple
import Data.Text
import Text.RawString.QQ
import Control.Monad.IO.Class
import Data.List (sort)
import Models
import Effects
import BusinessLogic
import Control.Monad.Reader

runSqlite :: (MonadIO m) => DbAccess r -> ReaderT Settings m r
runSqlite program = do
  Sqlite dbFile <- asks databaseSettings  
  liftIO $ withConnection dbFile $ \conn ->  
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
                        , isDeleted INTEGER
                        )
                    |]
      sqliteInterpreterHelper conn g

    sqliteInterpreterHelper conn (Free (GetBudgetItemDefinitions includeDeleted g)) = do
      results <- queryNamed conn [r|SELECT id
                                       , description
                                       , amount
                                       , startDate
                                       , frequency
                                       , isDeleted
                                    FROM definitions
                                    WHERE (:includeDeleted = 1) OR (:includeDeleted = 0 AND isDeleted = 0)                                
                              |] [":includeDeleted" := ((if includeDeleted == IncludeDeleted then 1 else 0) :: Int)]
      sqliteInterpreterHelper conn (g results)

    sqliteInterpreterHelper conn (Free (GetBudgetItemInstances s e defs g)) =
      sqliteInterpreterHelper conn $ g $ getAllInstances s e defs

    sqliteInterpreterHelper conn (Free (CreateBudgetItemDefinition def@BudgetItemDefinition{..} g)) = do
      executeNamed conn [r| INSERT INTO definitions (description, amount, startDate, frequency, isDeleted)
                        VALUES (:description, :amount, :startDate, :frequency, :isDeleted)
                    |] [ ":description" := description
                       , ":amount" := amount
                       , ":startDate" := startDate
                       , ":frequency" := frequency
                       , ":isDeleted" := False ]
      rowId <- lastInsertRowId conn                    
      sqliteInterpreterHelper conn $ g $ def { definitionId = rowId }

    sqliteInterpreterHelper conn (Free (UpdateBudgetItemDefinition BudgetItemDefinition{..} g)) = do
      executeNamed conn [r| UPDATE definitions
                            SET description = :description
                              , amount = :amount
                              , startDate = :startDate
                              , frequency = :frequency
                              , isDeleted = :isDeleted
                            WHERE id = :definitionId
                        |] [ ":definitionId" := definitionId
                           , ":description" := description
                           , ":amount" := amount
                           , ":startDate" := startDate
                           , ":frequency" := frequency
                           , ":isDeleted" := isDeleted ]
      sqliteInterpreterHelper conn g

    sqliteInterpreterHelper _ (Pure r) = return r