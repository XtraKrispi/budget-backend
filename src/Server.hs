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
import           Network.Wai.Handler.Warp
import           Control.Monad.IO.Class
import           Servant
import           Models
import           Effects
import           Sqlite
import           Data.Maybe
import           Data.Time.Clock

date :: IO Day
date = getCurrentTime >>= return . utctDay

type BudgetItemDefinitionApi =
  "definitions" :> Get '[JSON] [BudgetItemDefinition]

type BudgetItemInstanceApi =
  "instances" :> QueryParam "startDate" StartDate :> QueryParam "endDate" EndDate :>Get '[JSON] [BudgetItemInstance]

type ApplicationApi =
  BudgetItemDefinitionApi :<|> BudgetItemInstanceApi

definitionServer :: Server BudgetItemDefinitionApi
definitionServer = runDb (getBudgetItemDefinitions ExcludeDeleted)

instanceServer :: Server BudgetItemInstanceApi
instanceServer = getInstances
 where
  getInstances startDate endDate = do
    defStartDate <- liftIO date
    let defEndDate = defStartDate
    runDb $ getBudgetItemInstances (fromMaybe defStartDate startDate)
                                   (fromMaybe defEndDate endDate)

applicationServer :: Server ApplicationApi
applicationServer = definitionServer :<|> instanceServer

definitionApi :: Proxy BudgetItemDefinitionApi
definitionApi = Proxy

instanceApi :: Proxy BudgetItemInstanceApi
instanceApi = Proxy

applicationApi :: Proxy ApplicationApi
applicationApi = Proxy

app :: Application
app = serve applicationApi applicationServer

