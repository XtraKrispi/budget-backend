{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import Data.Aeson.Types
import Data.Time.Calendar
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Control.Monad.IO.Class
import Servant
import Models
import Effects
import Sqlite

type BudgetItemDefinitionApi = 
  "definitions" :> Get '[JSON] [BudgetItemDefinition]

definitionServer :: Server BudgetItemDefinitionApi
definitionServer = 
  runDb getBudgetItemDefinitions

definitionApi :: Proxy BudgetItemDefinitionApi
definitionApi = Proxy

app :: Application
app = serve definitionApi definitionServer

