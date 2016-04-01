{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
module Api
    ( app
    ) where

import Control.Monad.Reader             (ReaderT, runReaderT)
import Control.Monad.Trans.Except       (ExceptT)
import Database.Persist.Postgresql      (selectList, Entity(..))
import Network.Wai                      (Application)
import Servant

import Config
import Models


-- Application
type AppM = ReaderT Config (ExceptT ServantErr IO)

app :: Config -> Application
app cfg = serve api (readerServer cfg)

readerServer :: Config -> Server API
readerServer cfg = enter (readerToExcept cfg) server

readerToExcept :: Config -> AppM :~> ExceptT ServantErr IO
readerToExcept cfg = Nat $ \x -> runReaderT x cfg


-- API
api :: Proxy API
api = Proxy

type API = "users" :> Get '[JSON] [Entity User]

server :: ServerT API AppM
server = users

users :: AppM [Entity User]
users = runDB $ selectList [] []
