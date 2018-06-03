{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Users where

import Control.Monad.Except
import qualified Crypto.Hash.SHA256 as H
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as CL
import Data.Proxy
import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Database
import Database.Persist
import Database.Persist.Postgresql
import Servant
import Servant.Server.Experimental.Auth
import Web.JWT (Secret)

import Auth (hashPassword, signToken)
import Schema
import Types

type UsersAPI
   = "profile" :> AuthProtect "JWT" :> Get '[ JSON] UserProfile :<|> "login" :> ReqBody '[ JSON] Login :> Post '[ JSON] ReturnToken :<|> "register" :> ReqBody '[ JSON] Register :> Post '[ JSON] ReturnToken

-- | We need to specify the data returned after authentication
type instance AuthServerData (AuthProtect "JWT") = User

usersAPI :: Proxy UsersAPI
usersAPI = Proxy :: Proxy UsersAPI

usersServer :: Secret -> ConnectionString -> Server UsersAPI
usersServer secretKey connString =
  fetchUserProfileHandler :<|> loginHandler secretKey connString :<|>
  registerUserHandler secretKey connString

fetchUserProfileHandler :: User -> Handler UserProfile
fetchUserProfileHandler User {..} =
  return
    UserProfile
      { proEmail = userEmail
      , proChips = userChips
      , proUsername = Username userUsername
      }

------------------------------------------------------------------------
-- | Handlers
loginHandler :: Secret -> ConnectionString -> Login -> Handler ReturnToken
loginHandler secretKey conn Login {..} = do
  maybeUser <- liftIO $ dbGetUserByLogin conn loginWithHashedPswd
  maybe (throwError unAuthErr) createToken maybeUser
  where
    unAuthErr = err401 {errBody = "Incorrect email or password"}
    createToken (Entity _ User {..}) = signToken secretKey userEmail
    loginWithHashedPswd = Login {loginPassword = hashPassword loginPassword, ..}

-- when we register new user we check to see if email and username are already taken
-- if they are then the exception will be propagated to the client
registerUserHandler ::
     Secret -> ConnectionString -> Register -> Handler ReturnToken
registerUserHandler secretKey connString Register {..} = do
  let hashedPassword = hashPassword newUserPassword
  let (Username username) = newUsername
  let newUser =
        User
          { userUsername = username
          , userEmail = newUserEmail
          , userPassword = hashedPassword
          , userChips = 3000
          }
  registrationResult <- liftIO $ runExceptT $ dbRegisterUser connString newUser
  case registrationResult of
    Left err -> throwError $ err401 {errBody = CL.pack $ T.unpack err}
    _ -> signToken secretKey newUserEmail
