{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Models where

import           Data.Time.Calendar
import           Database.SQLite.Simple
import           Database.SQLite.Simple.Ok
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.ToField
import           Data.Text
import           GHC.Generics
import           Data.Aeson.Types
import           Data.Int
import           Control.Monad.Reader

type StartDate = Day
type EndDate = Day

data Frequency =
    OneTime
  | Weekly
  | Monthly
  | Yearly
  | BiWeekly
  deriving (Eq, Show, Read, Generic)

data BudgetItemDefinition = BudgetItemDefinition
  { definitionId :: Int64
  , description :: Text
  , amount :: Double
  , startDate :: Day
  , frequency :: Frequency
  , isDeleted :: Bool
  } deriving (Show, Eq, Generic)

instance ToJSON Frequency
instance ToJSON BudgetItemDefinition
instance FromJSON Frequency
instance FromJSON BudgetItemDefinition

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

instance ToJSON BudgetItemInstance where
  toJSON (BudgetItemInstance (def, d)) =
    object ["definition" .= def, "day" .= d]

data DatabaseSettings = Sqlite String

data Settings = Settings { databaseSettings :: DatabaseSettings
                         }