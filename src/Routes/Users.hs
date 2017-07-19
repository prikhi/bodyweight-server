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

import Control.Monad.Reader (liftIO, unless)
import Crypto.BCrypt
import Data.Aeson (FromJSON)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Database.Persist.Postgresql
import GHC.Generics
import Servant

import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID4

import Auth (lookupUser)
import Models
import Server (AppM, runDB, servantError, notFound)


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

-- | The expected data when reauthorizing a User.
data ReauthData
    = ReauthData
    { authToken :: T.Text
    , authUserId :: Key User
    } deriving (Generic, Show)

instance FromJSON ReauthData


type UserAPI =
         "register"
         :> ReqBody '[JSON] RegistrationData
         :> Post '[JSON] (JSONObject (Entity User))
    :<|> "login"
         :> ReqBody '[JSON] LoginData
         :> Post '[JSON] (JSONObject (Entity User))
    :<|> "reauthorize"
         :> ReqBody '[JSON] ReauthData
         :> Post '[JSON] (JSONObject (Entity User))


type UserRoutes =
         (RegistrationData -> AppM (JSONObject (Entity User)))
    :<|> (LoginData -> AppM (JSONObject (Entity User)))
    :<|> (ReauthData -> AppM (JSONObject (Entity User)))


userRoutes :: UserRoutes
userRoutes =
         registrationRoute
    :<|> loginRoute
    :<|> reauthorizeRoute


-- | Register a User by creating a User & Token for them.
registrationRoute :: RegistrationData -> AppM (JSONObject (Entity User))
registrationRoute data_ = do
    pass <- hashTextPassword (registrationPassword data_) >>=
                maybe (servantError $ err500 { errBody = "Misconfigured Salt" })
                (return)
    token <- UUID.toText <$> liftIO UUID4.nextRandom
    let user = User (registrationName data_) (registrationEmail data_)
                    pass token False
    userId <- runDB $ insert user
    return . JSONObject $ Entity userId user


-- | Log a user in by validating their password & returning their User data.
loginRoute :: LoginData -> AppM (JSONObject (Entity User))
loginRoute LoginData { loginName, loginPassword } = do
    maybeUser <- runDB . getBy $ UniqueUserName loginName
    case maybeUser of
        Nothing ->
            notFound
        Just userEntity@(Entity _ user) ->
            let
                isValidPassword =
                    validatePassword (encodeUtf8 $ userEncryptedPassword user)
                        (encodeUtf8 loginPassword)
            in
                if isValidPassword then
                    rehashPassword userEntity >> return (JSONObject userEntity)
                else
                    notFound
    where -- Rehash the password & Update the User if the saved hash uses
          -- an older HashingPolicy.
          rehashPassword (Entity userId user) =
            let
                hashedPassword =
                    encodeUtf8 $ userEncryptedPassword user

                updateUser newHash =
                    runDB . replace userId $ user { userEncryptedPassword = newHash }
            in
                unless (hashUsesPolicy slowerBcryptHashingPolicy hashedPassword) $
                    hashTextPassword loginPassword >>= (maybe (return ()) updateUser)


-- | Re-Authorize a User using their Auth Token.
reauthorizeRoute :: ReauthData -> AppM (JSONObject (Entity User))
reauthorizeRoute ReauthData { authToken, authUserId } = do
    userEntity@(Entity userId _) <- lookupUser authToken
    if authUserId == userId then
        return $ JSONObject userEntity
    else
        notFound


-- | Try to hash a plain text password using the `slowerBcryptHashingPolicy`.
hashTextPassword :: T.Text -> AppM (Maybe T.Text)
hashTextPassword =
    liftIO . fmap (fmap decodeUtf8) .
        hashPasswordUsingPolicy slowerBcryptHashingPolicy .
        encodeUtf8
