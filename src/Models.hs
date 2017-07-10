{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Models where

import Control.Monad                (mzero, unless)
import Control.Monad.Reader         (ReaderT, MonadIO, lift)
import Control.Monad.Trans.Except   (throwE)
import Data.Aeson                   (FromJSON(..), ToJSON(..), (.=), (.:),
                                     object, Value(..))
import Data.Proxy                   (Proxy(..))
import Data.Time.Calendar           (Day)
import Data.Time.LocalTime          (TimeOfDay)
import Database.Persist.Postgresql  (PersistEntityBackend, PersistEntity,
                                     SqlBackend(..), runMigration, Entity(..),
                                     EntityField, Key, deleteWhere, get,
                                     selectList, Filter, selectKeysList,
                                     (==.), (||.), (<-.))
import Database.Persist.TH          (share, mkPersist, sqlSettings, mkMigrate,
                                     persistLowerCase)
import Servant                      (err403, err404)

import qualified Data.Text       as T

import Types

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    name T.Text
    email T.Text
    encryptedPassword T.Text
    authToken T.Text
    isAdmin Bool default=False
    UniqueToken authToken
    UniqueUserName name

Subscription json
    user UserId
    routine RoutineId
    UniqueSubscription user routine

Routine json
    name T.Text
    author UserId
    isPublic Bool
    description T.Text
    copyright T.Text
    UniqueRoutine name

Section json
    name T.Text
    description T.Text
    routine RoutineId
    order Int
    UniqueSection name routine

SectionExercise json
    order Int
    section SectionId
    exercises [ExerciseId]
    setCount Int
    repCount Int
    holdTime Int
    repsToProgress Int
    timeToProgress Int
    restAfter Bool

Exercise json
    name T.Text
    description T.Text
    isHold Bool
    youtubeIds T.Text
    amazonIds T.Text
    copyright T.Text

RoutineLog json
    date Day
    routine RoutineId
    startTime TimeOfDay
    stopTime TimeOfDay
    startWeight Int
    stopWeight Int
    notes T.Text
    UniqueRoutineLog date routine

|]

doMigrations :: ReaderT SqlBackend IO ()
doMigrations = runMigration migrateAll


-- | Hide the encrypted password when converting a User to JSON.
instance ToJSON (Entity User) where
    toJSON (Entity userId user) =
        object
            [ "email" .= userEmail user
            , "authToken" .= userAuthToken user
            , "name" .= userName user
            , "isAdmin" .= userIsAdmin user
            , "id" .= userId
            ]



class Named a where
        name :: Proxy a -> T.Text

instance Named User where name _ = "user"
instance Named Subscription where name _ = "subscription"
instance Named Routine where name _ = "routine"
instance Named Section where name _ = "section"
instance Named SectionExercise where name _ = "sectionExercise"
instance Named Exercise where name _ = "exercise"
instance Named RoutineLog where name _ = "routineLog"

instance Named a => Named (Entity a) where
        name _ = name (Proxy :: Proxy a)


newtype JSONList a = JSONList [a]
instance (FromJSON a, Named a) => FromJSON (JSONList a) where
        parseJSON (Object o) = do
            named <- o .: name (Proxy :: Proxy a) >>= parseJSON
            return $ JSONList [named]
        parseJSON _          = mzero
instance (ToJSON a, Named a) => ToJSON (JSONList a) where
        toJSON (JSONList l)  = object [name (Proxy :: Proxy a) .= map toJSON l]

newtype JSONObject a = JSONObject a
instance (FromJSON a, Named a) => FromJSON (JSONObject a) where
        parseJSON (Object o) = do
            named <- o .: name (Proxy :: Proxy a) >>= parseJSON
            return $ JSONObject named
        parseJSON _          = mzero
instance (ToJSON a, Named a) => ToJSON (JSONObject a) where
        toJSON (JSONObject a)  = object [name (Proxy :: Proxy a) .= toJSON a]


class DeleteRelated a where
        deleteRelated :: (MonadIO m) => Key a -> ReaderT SqlBackend m ()
        deleteRelated _ = return ()

instance DeleteRelated Subscription
instance DeleteRelated SectionExercise
instance DeleteRelated Exercise
instance DeleteRelated RoutineLog

instance DeleteRelated User where
        deleteRelated key =
            deleteWhere [SubscriptionUser ==. key]
instance DeleteRelated Routine where
        deleteRelated key = do
            sections <- selectList [SectionRoutine ==. key] []
            mapM_ (\(Entity k _) -> deleteRelated k) sections
            deleteWhere [SectionRoutine ==. key]
instance DeleteRelated Section where
        deleteRelated key =
            deleteWhere [SectionExerciseSection ==. key]


-- | The GuardCRUD Typeclass is used to verify the user has the correct
-- permissions to perform the specified action for each CRUD Route.
class GuardCRUD a where

    -- | Guard the Create Route. The default instace will simply ensure
    -- a registered user is performing the action.
    guardCreate :: a -> Maybe (Entity User) -> AppM ()
    guardCreate _ =
        maybe forbidden (const $ return ())

    -- | Guard the Update Route. The default instance will simply use the
    -- guardCreate Route.
    guardUpdate :: Entity a -> Maybe (Entity User) -> AppM ()
    guardUpdate (Entity _ a) =
        guardCreate a

    -- | Guard the Delete Route. The default instance will only allow Admin
    -- users to delete the Resource.
    guardDelete :: Entity a -> Maybe (Entity User) -> AppM ()
    guardDelete _ Nothing =
        forbidden
    guardDelete _ (Just (Entity _ user)) =
        unless (userIsAdmin user) forbidden

    -- | Guard the List Route by returning a list of Filters to apply to
    -- the `selectList` query. The default instance does no filtering.
    guardList :: Maybe (Entity User) -> AppM [Filter a]
    guardList _ =
        return []

    -- | Guard the View Route. The default instance will allow anyone to
    -- see the details of a Resource.
    guardView :: Entity a -> Maybe (Entity User) -> AppM ()
    guardView _ _ =
        return ()

instance GuardCRUD Subscription
instance GuardCRUD RoutineLog

instance GuardCRUD Routine where
    guardUpdate _ Nothing =
        forbidden
    -- | Only Authors & Admins should be able to Update their Routines.
    guardUpdate (Entity routineId _) (Just user) =
        userIsAuthorOrAdmin user . runDB $ get routineId

    guardDelete _ Nothing =
        forbidden
    -- | Only Authors & Admins should be able to Delete a Routine.
    guardDelete (Entity _ routine) (Just (Entity userId user)) =
        unless (routineAuthor routine == userId || userIsAdmin user) forbidden

    -- | Anonymous Users should only see Public Routines.
    guardList Nothing =
        return [ RoutineIsPublic ==. True ]
    -- | Authorized Users should be shown all Public Routines as well as
    -- their Private Routines & Administrators should be shown all
    -- Routines.
    guardList (Just (Entity userId user)) =
        if userIsAdmin user then
            return []
        else
            return $
                [ RoutineIsPublic ==. True ] ||.
                [ RoutineIsPublic ==. False, RoutineAuthor ==. userId ]

    -- | Anonymous Users cannot view Private Routines.
    guardView (Entity _ routine) Nothing =
        unless (routineIsPublic routine) forbidden
    -- | Authorized Users can view their own Private Routines, while
    -- Administrators can view all Routines.
    guardView (Entity _ routine) (Just (Entity userId user)) =
        if userIsAdmin user then
            return ()
        else
            unless (routineIsPublic routine || routineAuthor routine == userId)
                forbidden

instance GuardCRUD Section where
    guardCreate _ Nothing =
        forbidden
    -- | Only Routine Authors or Admins should be able to Create Sections
    -- for a Routine.
    guardCreate section (Just user) =
        userIsAuthorOrAdmin user . runDB . get $ sectionRoutine section

    -- | Only Routine Authors & Admins should be able to Delete Sections
    -- for a Routine.
    guardDelete (Entity _ section) =
        guardCreate section

    -- | Defer filtering to the Routine Resource's `guardList`
    -- implementation.
    guardList =
        guardListByRelation SectionRoutine

    -- | Defer permission check to the Routine Resource's `guardView'
    -- implementation.
    guardView (Entity _ section) maybeUser =
        guardViewByRelation maybeUser $ sectionRoutine section

instance GuardCRUD SectionExercise where
    guardCreate _ Nothing =
        forbidden
    -- | Only Routine Authors or Admins should be able to Create
    -- SectionExercises for their Routines.
    guardCreate sectionExercise (Just user) = do
        maybeSection <- runDB . get $ sectionExerciseSection sectionExercise
        userIsAuthorOrAdmin user $ case maybeSection of
            Nothing ->
                return Nothing
            Just section ->
                runDB . get $ sectionRoutine section

    -- | Only Routine Authors or Admins should be able to Delete
    -- SectionExercises for a Routine.
    guardDelete (Entity _ sectionExercise) =
        guardCreate sectionExercise

    -- | Defer filtering to the Section Resource's `guardList`
    -- implementation(which defers to Routine's `guardList` implementation).
    guardList =
        guardListByRelation SectionExerciseSection

    -- | Defer permission check to the Section Resource's `guardView`
    -- implementation.
    guardView (Entity _ sectionExercise) maybeUser =
        guardViewByRelation maybeUser $ sectionExerciseSection sectionExercise

instance GuardCRUD Exercise where
    -- | Only Admins should be able to Create Exercises
    guardCreate _ Nothing =
        forbidden
    guardCreate _ (Just (Entity _ user)) =
        unless (userIsAdmin user) forbidden

-- | A helper function that can be used when a guard should return a 403
-- error.
forbidden :: AppM b
forbidden =
    lift $ throwE err403

-- | A helper function to verify a User is a Routine's Author or an Admin.
userIsAuthorOrAdmin :: Entity User -> AppM (Maybe Routine) -> AppM ()
userIsAuthorOrAdmin (Entity userId user) maybeRoutineM = do
    maybeRoutine <- maybeRoutineM
    case maybeRoutine of
        Nothing ->
            lift $ throwE err404
        Just routine ->
            unless (routineAuthor routine == userId || userIsAdmin user) forbidden

-- | A helper function to defer guarding a List route to a Relation's guard.
guardListByRelation :: ( PersistEntityBackend r ~ SqlBackend, PersistEntity r
                       , GuardCRUD r)
                    => EntityField v (Key r) -> Maybe (Entity User) -> AppM [Filter v]
guardListByRelation foreignKey maybeUser = do
    relationFilters <- guardList maybeUser
    ids <- runDB $ selectKeysList relationFilters []
    return [ foreignKey <-. ids ]

-- | A helper function to defer guarding a View route to a Related Resource.
guardViewByRelation :: ( PersistEntityBackend r ~ SqlBackend, PersistEntity r
                       , GuardCRUD r )
                    => Maybe (Entity User) -> Key r -> AppM ()
guardViewByRelation maybeUser foreignKey = do
    maybeItem <- runDB $ get foreignKey
    case maybeItem of
        Nothing ->
            lift $ throwE err404
        Just item ->
            guardView (Entity foreignKey item) maybeUser
