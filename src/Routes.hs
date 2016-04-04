{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module Routes
    ( CRUD
    , CRUDRoutes
    , crudRoutes
    ) where
import Control.Monad.Reader         (lift)
import Control.Monad.Trans.Except   (throwE)
import Database.Persist.Postgresql
import Servant

import Models
import Types


type CRUD resource =
        Get '[JSON] (JSONList (Entity resource))
   :<|> ReqBody '[JSON] (JSONObject resource)
        :> Post '[JSON] (JSONObject (Entity resource))
   :<|> Capture "id" (Key resource)
        :> Get '[JSON] (JSONObject (Entity resource))
   :<|> Capture "id" (Key resource)
        :> ReqBody '[JSON] (JSONObject resource)
        :> Put '[JSON] (JSONObject (Entity resource))
   :<|> Capture "id" (Key resource)
        :> Delete '[JSON] ()

type CRUDRoutes resource =
         AppM (JSONList (Entity resource))
    :<|> ((JSONObject resource -> AppM (JSONObject (Entity resource)))
    :<|> ((Key resource -> AppM (JSONObject (Entity resource)))
    :<|> ((Key resource -> JSONObject resource -> AppM (JSONObject (Entity resource)))
    :<|> (Key resource -> AppM ()))))

crudRoutes :: (PersistEntityBackend r ~ SqlBackend, PersistEntity r,
               ToBackendKey SqlBackend r)
           => CRUDRoutes r
crudRoutes =    listRoute
           :<|> createRoute
           :<|> viewRoute
           :<|> updateRoute
           :<|> deleteRoute

-- | The `listRoute` returns a JSON Array of Persistent Entities.
listRoute :: (PersistEntityBackend r ~ SqlBackend, PersistEntity r)
          => AppM (JSONList (Entity r))
listRoute = do items <- runDB (selectList [] [])
               return $ JSONList items

-- | The `createRoute` parses a JSON request body & inserts the value into
-- the database if it is valid.
createRoute :: (PersistEntityBackend r ~ SqlBackend, PersistEntity r)
          => JSONObject r -> AppM (JSONObject (Entity r))
createRoute (JSONObject item) = do
        key <- runDB $ insert item
        return . JSONObject $ Entity key item

-- | The `viewRoute` returns a single JSON object representing a Persistent
-- Entity.
viewRoute :: (PersistEntityBackend r ~ SqlBackend, PersistEntity r,
              ToBackendKey SqlBackend r)
          => Key r -> AppM (JSONObject (Entity r))
viewRoute key =  do
        maybeItem <- runDB $ get key
        case maybeItem of
            Nothing -> lift $ throwE err404
            Just value -> return $ JSONObject (Entity key value)

-- | The `updateRoute` attempts to update an Entity using a JSON request
-- body and returns the new Entity.
updateRoute :: (PersistEntityBackend r ~ SqlBackend, PersistEntity r,
                ToBackendKey SqlBackend r)
            => Key r -> JSONObject r -> AppM (JSONObject (Entity r))
updateRoute key (JSONObject item) = do
        runDB $ replace key item
        return . JSONObject $ Entity key item

-- | The `deleteRoute` deletes the Entity, if it exists.
deleteRoute :: (PersistEntityBackend r ~ SqlBackend, PersistEntity r,
                ToBackendKey SqlBackend r)
            => Key r -> AppM ()
deleteRoute key = do
        maybeItem <- runDB $ get key
        case maybeItem of
            Nothing -> lift $ throwE err404
            _       -> runDB $ delete key
