{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Models where

import Control.Monad.IO.Class       (MonadIO)
import Control.Monad.Reader         (ReaderT, asks, liftIO, MonadReader)
import Database.Persist.Postgresql  (SqlBackend(..), runMigration, runSqlPool,
                                     SqlPersistT)
import Database.Persist.TH          (share, mkPersist, sqlSettings, mkMigrate,
                                     persistLowerCase)

import Config


runDB :: (MonadIO m, MonadReader Config m) => SqlPersistT IO b -> m b
runDB query = do
    pool <- asks getPool
    liftIO $ runSqlPool query pool

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User json
    name String
    email String
    deriving Show
|]

doMigrations :: ReaderT SqlBackend IO ()
doMigrations = runMigration migrateAll
