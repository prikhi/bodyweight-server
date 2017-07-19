{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Auth where

import Data.Text.Encoding (decodeUtf8)
import Database.Persist.Postgresql
import Servant (err401, errBody, AuthProtect, Context(..))
import Servant.Server.Experimental.Auth (AuthHandler, mkAuthHandler, AuthServerData)
import Network.Wai (Request, requestHeaders)

import qualified Data.Text as T

import Models
import Server   (AppM, runDB, servantError)


-- | A Token is representing by Text.
type TokenString = T.Text


-- | Retrieve the User for a given Token.
lookupUser :: TokenString -> AppM (Entity User)
lookupUser tokenString =
    (runDB . getBy $ UniqueToken tokenString) >>=
    maybe (servantError $ err401 { errBody = "Invalid Token" }) return


-- | Pull an Optional Token from the `Auth-Token` Header.
authHandler :: AuthHandler Request (Maybe TokenString)
authHandler =
    let
        handler request =
            return . fmap decodeUtf8 $ lookup "Auth-Token" (requestHeaders request)
    in
        mkAuthHandler handler


-- | The Token AuthHandler is denoted by the `AuthProtect "token-auth"`
-- combinator.
type instance AuthServerData (AuthProtect "token-auth") = Maybe TokenString


-- | Create a Context with the Token AuthHandler.
authServerContext :: Context (AuthHandler Request (Maybe TokenString) ': '[])
authServerContext = authHandler :. EmptyContext
