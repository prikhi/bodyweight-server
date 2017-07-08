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

import Auth
import Models
import Types


type CRUD resource =
        Get '[JSON] (JSONList (Entity resource))
   :<|> AuthProtect "token-auth"
        :> ReqBody '[JSON] (JSONObject resource)
        :> Post '[JSON] (JSONObject (Entity resource))
   :<|> Capture "id" (Key resource)
        :> Get '[JSON] (JSONObject (Entity resource))
   :<|> AuthProtect "token-auth"
        :> Capture "id" (Key resource)
        :> ReqBody '[JSON] (JSONObject resource)
        :> Put '[JSON] (JSONObject (Entity resource))
   :<|> AuthProtect "token-auth"
        :> Capture "id" (Key resource)
        :> Delete '[JSON] ()

type CRUDRoutes resource =
         AppM (JSONList (Entity resource))
    :<|> ((Maybe TokenString -> JSONObject resource -> AppM (JSONObject (Entity resource)))
    :<|> ((Key resource -> AppM (JSONObject (Entity resource)))
    :<|> ((Maybe TokenString -> Key resource -> JSONObject resource -> AppM (JSONObject (Entity resource)))
    :<|> (Maybe TokenString -> Key resource -> AppM ()))))

crudRoutes :: ( PersistEntityBackend r ~ SqlBackend, ToBackendKey SqlBackend r
              , DeleteRelated r, GuardCRUD r)
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
createRoute :: ( PersistEntityBackend r ~ SqlBackend, PersistEntity r
               , GuardCRUD r )
          => Maybe TokenString -> JSONObject r -> AppM (JSONObject (Entity r))
createRoute maybeToken (JSONObject item) = do
        mapTokenToGuard maybeToken $ guardCreate item
        key <- runDB $ insert item
        return . JSONObject $ Entity key item

-- | The `viewRoute` returns a single JSON object representing a Persistent
-- Entity.
viewRoute :: (PersistEntityBackend r ~ SqlBackend, ToBackendKey SqlBackend r)
          => Key r -> AppM (JSONObject (Entity r))
viewRoute key =  do
        maybeItem <- runDB $ get key
        case maybeItem of
            Nothing -> lift $ throwE err404
            Just value -> return $ JSONObject (Entity key value)

-- | The `updateRoute` attempts to update an Entity using a JSON request
-- body and returns the new Entity.
updateRoute :: ( PersistEntityBackend r ~ SqlBackend, ToBackendKey SqlBackend r
               , GuardCRUD r )
            => Maybe TokenString -> Key r -> JSONObject r -> AppM (JSONObject (Entity r))
updateRoute maybeToken key (JSONObject item) = do
        mapTokenToGuard maybeToken $ guardUpdate (Entity key item)
        runDB $ replace key item
        return . JSONObject $ Entity key item

-- | The `deleteRoute` deletes the Entity, if it exists.
deleteRoute :: ( PersistEntityBackend r ~ SqlBackend, ToBackendKey SqlBackend r
               , DeleteRelated r, GuardCRUD r)
            => Maybe TokenString -> Key r -> AppM ()
deleteRoute maybeToken key = do
        maybeItem <- runDB $ get key
        case maybeItem of
            Nothing ->
                lift $ throwE err404
            Just item ->
                mapTokenToGuard maybeToken (guardDelete $ Entity key item) >>
                runDB (deleteRelated key >> delete key)


-- | Map a Potential Token onto a Guard Function
mapTokenToGuard :: Maybe TokenString -> (Maybe (Entity User) -> AppM ()) -> AppM ()
mapTokenToGuard maybeToken guardFunction =
        case maybeToken of
            Nothing ->
                guardFunction Nothing
            Just token ->
                lookupUser token >>= guardFunction . Just
