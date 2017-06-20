{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Routes.Users
    ( UserAPI
    , UserRoutes
    , userRoutes
    ) where

import Control.Monad.Reader (lift, liftIO)
import Control.Monad.Trans.Except   (throwE)
import Crypto.BCrypt
import Data.Aeson (FromJSON)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Database.Persist.Postgresql
import GHC.Generics
import Servant

import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID4

import Types
import Models


-- | The expected data when registering a User.
data RegistrationData
    = RegistrationData
    { registrationName :: T.Text
    , registrationEmail :: T.Text
    , registrationPassword :: T.Text
    } deriving (Generic, Show)

instance FromJSON RegistrationData


-- | The expected data when logging a User in.
data LoginData
    = LoginData
    { loginName :: T.Text
    , loginPassword :: T.Text
    } deriving (Generic, Show)

instance FromJSON LoginData


type UserAPI =
         "register"
         :> ReqBody '[JSON] RegistrationData
         :> Post '[JSON] (JSONObject (Entity User))
    :<|> "login"
         :> ReqBody '[JSON] LoginData
         :> Post '[JSON] (JSONObject (Entity User))


type UserRoutes =
         (RegistrationData -> AppM (JSONObject (Entity User)))
    :<|> (LoginData -> AppM (JSONObject (Entity User)))


userRoutes :: UserRoutes
userRoutes =
         registrationRoute
    :<|> loginRoute


-- | Register a User by creating a User & Token for them.
registrationRoute :: RegistrationData -> AppM (JSONObject (Entity User))
registrationRoute data_ = do
    pass <- do
        maybePass <- liftIO . encryptPassword . encodeUtf8 $ registrationPassword data_
        case maybePass of
            Nothing ->
                lift . throwE $ err500 { errBody = "Misconfigured Salt" }
            Just pass ->
                return $ decodeUtf8 pass
    token <- UUID.toText <$> liftIO UUID4.nextRandom
    let user = User (registrationName data_) (registrationEmail data_)
                    pass token False
    userId <- runDB $ insert user
    return . JSONObject $ Entity userId user
    where encryptPassword =
            hashPasswordUsingPolicy slowerBcryptHashingPolicy


-- | Log a user in by validating their password & returning their User data.
loginRoute :: LoginData -> AppM (JSONObject (Entity User))
loginRoute LoginData { loginName, loginPassword } = do
        maybeUser <- runDB . getBy $ UniqueUserName loginName
        case maybeUser of
            Nothing ->
                lift . throwE $ err404
            Just userEntity@(Entity _ user) ->
                let
                    validPassword =
                        validatePassword (encodeUtf8 $ userEncryptedPassword user)
                            (encodeUtf8 loginPassword)
                in
                    if validPassword then
                        return . JSONObject $ userEntity
                    else
                        lift . throwE $ err404
