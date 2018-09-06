{-# LANGUAGE DeriveFunctor #-}
module Effects where

import Control.Monad.Free
import Database.SQLite.Simple
import Text.RawString.QQ

import Models

data Effect next =
    CreateDatabase next
  | GetBudgetItemDefinitions ([BudgetItemDefinition] -> next)
  | GetBudgetItemInstances StartDate EndDate ([BudgetItemInstance] -> next)
  deriving (Functor)

type DbAccess = Free Effect

createDatabase :: DbAccess ()
createDatabase = liftF (CreateDatabase ())

getBudgetItemDefinitions :: DbAccess [BudgetItemDefinition]
getBudgetItemDefinitions = liftF (GetBudgetItemDefinitions id)

getBudgetItemInstances :: StartDate -> EndDate -> DbAccess [BudgetItemInstance]
getBudgetItemInstances s e = liftF (GetBudgetItemInstances s e id)