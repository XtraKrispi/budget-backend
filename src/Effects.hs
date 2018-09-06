{-# LANGUAGE DeriveFunctor #-}
module Effects where

import           Control.Monad.Free
import           Database.SQLite.Simple
import           Text.RawString.QQ

import           Models

data InclusionRule = IncludeDeleted | ExcludeDeleted
  deriving (Show, Eq)

data Effect next =
    CreateDatabase next
  | GetBudgetItemDefinitions InclusionRule ([BudgetItemDefinition] -> next)
  | GetBudgetItemInstances StartDate EndDate [BudgetItemDefinition] ([BudgetItemInstance] -> next)
  deriving (Functor)

type DbAccess = Free Effect

createDatabase :: DbAccess ()
createDatabase = liftF (CreateDatabase ())

getBudgetItemDefinitions :: InclusionRule -> DbAccess [BudgetItemDefinition]
getBudgetItemDefinitions includeDeleted =
  liftF (GetBudgetItemDefinitions includeDeleted id)

getBudgetItemInstances :: StartDate -> EndDate -> DbAccess [BudgetItemInstance]
getBudgetItemInstances s e = do
  defs <- getBudgetItemDefinitions ExcludeDeleted
  liftF (GetBudgetItemInstances s e defs id)
