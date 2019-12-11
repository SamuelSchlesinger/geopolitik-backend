module Geopolitik.Server.Account (account) where

import Control.Monad.Except
import Data.Text.Encoding
import Data.Time.Clock
import Data.UUID
import Geopolitik.API
import Geopolitik.Database
import Geopolitik.Ontology.User
import Geopolitik.Ontology.Session
import Geopolitik.Ontology.Key
import Geopolitik.Monad
import Servant
import System.Random
import Web.Cookie

account :: MonadGeopolitik m => ServerT AccountAPI m
account = signup :<|> signin :<|> newToken

signup :: MonadGeopolitik m => SignUp -> m (Response SignUp)
signup (SignUp username password) = do
  lookupUsersByUsername [username] >>= \case
    [] -> do
      userCreationDate <- liftIO getCurrentTime
      userID <- liftIO ((Key . toText) <$> randomIO)
      insertUsers [User {..}]
      return SignedUp
    _ -> throwError err403 {errReasonPhrase = "Username already exists"}

signin :: MonadGeopolitik m => SignIn -> m (H (Response SignIn))
signin (SignIn username password) =
  lookupUsersByUsername [username] >>= \case
    [User {userID = sessionOwner, password = userPassword}] -> do
      if (password == userPassword)
        then do
          sessionCreationDate <- liftIO getCurrentTime
          sessionToken <- liftIO (toText <$> randomIO)
          sessionID <- liftIO ((Key . toText) <$> randomIO)
          let sess = Session {..}
          insertSessions [sess]
          return $
            addHeader
              (def
                 { setCookieName = "geopolitik-user"
                 , setCookieSameSite = Just sameSiteStrict
                 , setCookieMaxAge = Nothing
                 , setCookieHttpOnly = True
                 , setCookiePath = Just "/"
                 , setCookieValue = encodeUtf8 sessionToken
                 }) $
            addHeader "http://localhost:8080" $
            addHeader "Accept" $
            addHeader True $ SignedIn sessionToken
        else throwError err403 {errReasonPhrase = "Wrong password"}
    _ -> throwError err403 {errReasonPhrase = "Wrong username"}

newToken :: MonadGeopolitik m => User -> m (H (Response SignIn))
newToken User {..} = do
  signin $ SignIn username password
