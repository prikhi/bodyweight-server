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

import Control.Monad                (mzero, void)
import Control.Monad.Reader         (ReaderT, MonadIO)
import Data.Aeson                   (FromJSON(..), ToJSON(..), (.=), (.:),
                                     object, Value(..))
import Data.Proxy                   (Proxy(..))
import Data.Time.Calendar           (Day)
import Data.Time.LocalTime          (TimeOfDay)
import Database.Persist.Postgresql  (SqlBackend(..), runMigration, Entity(..),
                                     Key, deleteWhere, selectList, (==.))
import Database.Persist.TH          (share, mkPersist, sqlSettings, mkMigrate,
                                     persistLowerCase)
import qualified Data.Text       as T


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User json
    name T.Text
    email T.Text
    isAdmin Bool default=False

Subscription json
    user UserId
    routine RoutineId
    UniqueSubscription user routine

Routine json
    name T.Text
    author UserId Maybe
    isPublic Bool
    copyright T.Text
    UniqueRoutine name

Section json
    name T.Text
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


data JSONList a = JSONList [a]
instance (FromJSON a, Named a) => FromJSON (JSONList a) where
        parseJSON (Object o) = do
            named <- o .: name (Proxy :: Proxy a) >>= parseJSON
            return $ JSONList [named]
        parseJSON _          = mzero
instance (ToJSON a, Named a) => ToJSON (JSONList a) where
        toJSON (JSONList l)  = object [name (Proxy :: Proxy a) .= map toJSON l]

data JSONObject a = JSONObject a
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
            void $ mapM (\(Entity k _) -> deleteRelated k) sections
            deleteWhere [SectionRoutine ==. key]
instance DeleteRelated Section where
        deleteRelated key =
            deleteWhere [SectionExerciseSection ==. key]
