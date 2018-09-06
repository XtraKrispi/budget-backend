{-# LANGUAGE DeriveGeneric #-}
module Models where

import           Data.Time.Calendar
import           Database.SQLite.Simple
import           Database.SQLite.Simple.Ok
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.ToField
import           Data.Text
import           GHC.Generics
import           Data.Aeson.Types

type StartDate = Day
type EndDate = Day

data Frequency =
  OneTime
  | Weekly
  | Monthly
  | Yearly
  | SemiMonthly
  | BiWeekly
  deriving (Eq, Show, Read, Generic)

data BudgetItemDefinition = BudgetItemDefinition
  { definitionId :: Int
  , description :: Text
  , amount :: Double
  , startDate :: Day
  , frequency :: Frequency
  , isDeleted :: Bool
  } deriving (Show, Eq, Generic)

instance ToJSON Frequency
instance ToJSON BudgetItemDefinition

instance FromField Frequency where
  fromField field = do
    let SQLText d = fieldData field
    return . read . unpack $ d

instance ToField Frequency where
  toField = SQLText . pack . show

instance FromRow BudgetItemDefinition where
    fromRow = BudgetItemDefinition <$> field <*> field <*> field <*> field <*> field <*> field

newtype BudgetItemInstance = BudgetItemInstance (BudgetItemDefinition, Day)
  deriving (Generic)

instance ToJSON BudgetItemInstance
