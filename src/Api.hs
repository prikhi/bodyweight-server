{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
module Api
    ( app
    ) where

import Control.Monad.Reader             (runReaderT)
import Control.Monad.Trans.Except       (ExceptT)
import Network.Wai                      (Application)
import Servant

import Auth
import Config
import Models
import Routes
import Routes.Users
import Server


-- Application
app :: Config -> Application
app cfg = serveWithContext api authServerContext (readerServer cfg)

readerServer :: Config -> Server API
readerServer cfg = enter (readerToExcept cfg) server

readerToExcept :: Config -> AppM :~> ExceptT ServantErr IO
readerToExcept cfg = Nat $ \x -> runReaderT x cfg


-- API
api :: Proxy API
api = Proxy

type API = 
        "users" :> UserAPI
   :<|> "subscriptions" :> SubscriptionAPI
   :<|> "routines" :> RoutineAPI
   :<|> "sections" :> SectionAPI
   :<|> "sectionExercises" :> SectionExerciseAPI
   :<|> "exercises" :> ExerciseAPI
   :<|> "routineLogs" :> RoutineLogAPI

server :: ServerT API AppM
server = 
        userRoutes
   :<|> subscriptionRoutes
   :<|> routineRoutes
   :<|> sectionRoutes
   :<|> sectionExerciseRoutes
   :<|> exerciseRoutes
   :<|> routineLogRoutes

type SubscriptionAPI = CRUD Subscription
subscriptionRoutes :: CRUDRoutes Subscription
subscriptionRoutes = crudRoutes

type ExerciseAPI = CRUD Exercise
exerciseRoutes :: CRUDRoutes Exercise
exerciseRoutes = crudRoutes

type RoutineAPI = CRUD Routine
routineRoutes :: CRUDRoutes Routine
routineRoutes = crudRoutes

type SectionAPI = CRUD Section
sectionRoutes :: CRUDRoutes Section
sectionRoutes = crudRoutes

type SectionExerciseAPI = CRUD SectionExercise
sectionExerciseRoutes :: CRUDRoutes SectionExercise
sectionExerciseRoutes = crudRoutes

type RoutineLogAPI = CRUD RoutineLog
routineLogRoutes :: CRUDRoutes RoutineLog
routineLogRoutes = crudRoutes
