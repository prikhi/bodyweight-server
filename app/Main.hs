module Main where

import Network.Wai.Handler.Warp         (run)
import System.Environment               (lookupEnv)
import Database.Persist.Postgresql      (runSqlPool)

import Config (defaultConfig, Config(..), Environment(..), setLogger, makePool)
import Api    (app)
import Models (doMigrations)


-- | Grab Settings From Environmental Variables, Connect to the Database,
-- & Launch the Web Server.
main :: IO ()
main = do
    env  <- lookupSetting "ENV" Development
    port <- lookupSetting "PORT" 8080
    pool <- makePool env
    let cfg = defaultConfig { getPool = pool, getEnv = env }
    runSqlPool doMigrations pool
    run port $ setLogger env $ app cfg
    where lookupSetting env def =
            maybe def read <$> lookupEnv env
