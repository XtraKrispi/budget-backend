{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import           Data.Aeson.Types
import           Data.Time.Calendar
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp hiding (Settings)
import           Control.Monad.IO.Class
import           Servant
import           Models
import           Effects
import           Sqlite
import           Data.Maybe
import           Data.Time.Clock
import           Data.Int
import           Control.Monad.Reader


date :: IO Day
date = utctDay <$> getCurrentTime

type AppM = ReaderT Settings Handler

type BudgetItemDefinitionApi =
      "definitions" :> Get '[JSON] [BudgetItemDefinition] 
 :<|> "definitions" :> ReqBody '[JSON] BudgetItemDefinition :> Post '[JSON] BudgetItemDefinition
 :<|> "definitions" :> Capture "id" Int64 :> ReqBody '[JSON] BudgetItemDefinition :> Put '[JSON] ()

type BudgetItemInstanceApi =
  "instances" :> QueryParam "startDate" StartDate :> QueryParam "endDate" EndDate :>Get '[JSON] [BudgetItemInstance]

type ApplicationApi =
  BudgetItemDefinitionApi :<|> BudgetItemInstanceApi

definitionServer :: ServerT BudgetItemDefinitionApi AppM
definitionServer = runSqlite (getBudgetItemDefinitions ExcludeDeleted)
              :<|> runSqlite . createBudgetItemDefinition
              :<|> \id def -> runSqlite (updateBudgetItemDefinition (def{ definitionId = id }))

instanceServer :: ServerT BudgetItemInstanceApi AppM
instanceServer = getInstances
 where
  getInstances startDate endDate = do
    defStartDate <- liftIO date
    let defEndDate = addDays 20 defStartDate
    runSqlite $ getBudgetItemInstances (fromMaybe defStartDate startDate)
                                   (fromMaybe defEndDate endDate)

applicationServer :: ServerT ApplicationApi AppM
applicationServer = definitionServer :<|> instanceServer

definitionApi :: Proxy BudgetItemDefinitionApi
definitionApi = Proxy

instanceApi :: Proxy BudgetItemInstanceApi
instanceApi = Proxy

applicationApi :: Proxy ApplicationApi
applicationApi = Proxy

nt :: Settings -> AppM a -> Handler a
nt = flip runReaderT

app :: Settings -> Application
app s = serve applicationApi $ hoistServer applicationApi (nt s) applicationServer

