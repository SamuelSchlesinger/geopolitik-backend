module Geopolitik.Server where

import Geopolitik.Database
import Geopolitik.Ontology
import Control.Monad.Trans
import Geopolitik.API
import Servant
import Servant.Server.Experimental.Auth
import Data.Time.Clock
import Data.Text.Encoding
import Data.UUID
import System.Random
import qualified Network.Wai as Wai
import Database.PostgreSQL.Simple hiding ((:.))
import Web.Cookie

type M = DatabaseT Handler

geopolitikProxy :: Proxy GeopolitikAPI
geopolitikProxy = Proxy

ctxProxy :: Proxy Ctx
ctxProxy = Proxy

ctx :: Connection -> Context Ctx
ctx conn = mkAuthHandler validate :. EmptyContext where
  validate :: Wai.Request -> Handler User
  validate req = do
    cookie <- maybe (throwError err403) return 
      $ lookup "cookie" (Wai.requestHeaders req)
    token <- maybe (throwError err403) return
      $ lookup "geopolitik-user" $ parseCookies cookie
    runSharedDatabaseT conn (validateToken token) >>= \case
      Just user -> return user
      Nothing -> throwError err401     

server :: ServerT GeopolitikAPI M
server = account :<|> article 

account :: ServerT AccountAPI M
account = signup :<|> signin

signup :: SignUp -> M (Response SignUp)
signup (SignUp username password) 
  = lookupUsersByUsername [username] >>= \case
      [] -> do
        userCreationDate <- liftIO getCurrentTime
        userID <- liftIO ((Key . toText) <$> randomIO)
        insertUsers [User{..}] 
        return SignedUp
      _ -> return SignUpFailure

signin :: SignIn -> M (Headers '[Header "Set-Cookie" SetCookie] (Response SignIn))
signin (SignIn username password)
  = lookupUsersByUsername [username] >>= \case
      [User{userID = sessionOwner, password = userPassword}] -> do
        if (password == userPassword) then do
          sessionCreationDate <- liftIO getCurrentTime
          sessionToken <- liftIO (toText <$> randomIO)      
          sessionID <- liftIO ((Key . toText) <$> randomIO) 
          let sess = Session{..}
          insertSessions [sess]
          return $ addHeader (def { setCookieName = "geopolitik-user", setCookieValue = encodeUtf8 sessionToken }) SignedIn 
        else throwError err403
      _ -> throwError err403

article :: ServerT ArticleAPI M
article user = newArticle user :<|> draft user

newArticle :: User -> NewArticle -> DatabaseT Handler (Response NewArticle)
newArticle User{userID = articleOwner} (NewArticle articleName) = do
  articleCreationDate <- liftIO getCurrentTime
  articleID <- liftIO ((Key . toText) <$> randomIO) 
  insertArticles [Article{..}]
  return ArticleCreated

draft :: User -> ServerT DraftAPI M
draft user = newDraft user :<|> latest user

newDraft :: User -> NewDraft -> M (Response NewDraft)
newDraft User{userID = draftAuthor} (NewDraft draftArticle draftContents) = do
  draftCreationDate <- liftIO getCurrentTime
  draftID <- liftIO ((Key . toText) <$> randomIO)
  insertDrafts [Draft{..}]
  return DraftCreated

latest :: User -> LatestDraft -> M (Response LatestDraft)
latest _ (LatestDraft articleKey) 
  = latestDraft articleKey >>= \case
      Just (Draft draftID _ contents draftAuthor timestamp) -> return $ LatestDraftFound draftID draftAuthor contents timestamp
      Nothing -> return LatestDraftNotFound