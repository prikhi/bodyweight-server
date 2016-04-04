{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
module Api
    ( app
    ) where

import Control.Monad.Reader             (runReaderT)
import Control.Monad.Trans.Except       (ExceptT)
import Network.Wai                      (Application)
import Servant

import Config
import Models
import Types
import Routes


-- Application
app :: Config -> Application
app cfg = serve api (readerServer cfg)

readerServer :: Config -> Server API
readerServer cfg = enter (readerToExcept cfg) server

readerToExcept :: Config -> AppM :~> ExceptT ServantErr IO
readerToExcept cfg = Nat $ \x -> runReaderT x cfg


-- API
api :: Proxy API
api = Proxy

type API = "users" :> UserAPI
      :<|> "subscriptions" :> SubscriptionAPI
      :<|> "exercises" :> ExerciseAPI

server :: ServerT API AppM
server = userRoutes
    :<|> subscriptionRoutes
    :<|> exerciseRoutes

type UserAPI = CRUD User
userRoutes :: CRUDRoutes User
userRoutes = crudRoutes

type SubscriptionAPI = CRUD Subscription
subscriptionRoutes :: CRUDRoutes Subscription
subscriptionRoutes = crudRoutes

type ExerciseAPI = CRUD Exercise
exerciseRoutes :: CRUDRoutes Exercise
exerciseRoutes = crudRoutes
