{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Server where

import Control.Monad.Reader         (ReaderT, asks, liftIO, lift)
import Control.Monad.Trans.Except   (ExceptT, throwE)
import Data.Aeson                   (FromJSON(..), ToJSON(..))
import Data.Int                     (Int64)
import Database.Persist.Postgresql  (SqlPersistT, runSqlPool, ToBackendKey, Key, toSqlKey, SqlBackend)
import Servant

import Config                       (Config(..))


-- | AppM represents the Monad Stack used in the Handlers. It is an IO
-- monad with Servant errors, wrapped in a Reader holding our Config
-- values.
type AppM =
    ReaderT Config (ExceptT ServantErr IO)

-- | AppSQL represents the lifting of SQL actions into our `AppM` monad.
type AppSQL a =
    SqlPersistT IO a -> AppM a


-- | Run an SQL action & return the type in our `AppM` monad.
runDB :: AppSQL a
runDB query =
    asks getPool >>= liftIO . runSqlPool query


-- | A Persistent Key wrapped in a newtype so that we can derive the
-- FromText & ToText instances necessary for Servant's routing.
newtype PKey a
    = PKey
    { fromPKey :: Int64
    } deriving (FromJSON, ToJSON)


-- | Retrieve a Persistent SQL Key out of a PKey value.
_PKey :: ToBackendKey SqlBackend a => PKey a -> Key a
_PKey pKey =
    toSqlKey $ fromPKey pKey


-- HTTP Error Helpers

-- | Lift a Servant Error into the `AppM` monad.
servantError :: ServantErr -> AppM a
servantError =
    lift . throwE

-- | Return a 403 Response.
forbidden :: AppM a
forbidden =
    servantError err403

-- | Return a 404 Response.
notFound :: AppM a
notFound =
    servantError err404
