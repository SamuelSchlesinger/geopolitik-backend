module Geopolitik.Server where

import Control.Monad.Reader.Class
import Control.Monad.Trans
import Data.Text (Text)
import Data.Text.Encoding
import Data.Time.Clock
import Data.UUID
import Database.PostgreSQL.Simple hiding ((:.))
import Database.PostgreSQL.Simple.SqlQQ
import Geopolitik.API
import Geopolitik.Auth
import Geopolitik.Database
import Geopolitik.Ontology
import qualified Network.Wai as Wai
import Servant
import Servant.Server.Experimental.Auth
import System.Random
import Web.Cookie

type M = DatabaseT Handler

geopolitikProxy :: Proxy GeopolitikAPI
geopolitikProxy = Proxy

ctxProxy :: Proxy Ctx
ctxProxy = Proxy

ctx :: Connection -> Context Ctx
ctx conn = mkAuthHandler validate :. EmptyContext
  where
    validate :: Wai.Request -> Handler User
    validate req = do
      cookie <-
        maybe
          (throwError err401 {errReasonPhrase = "You have no cookies"})
          return $
        lookup "cookie" (Wai.requestHeaders req)
      token <-
        maybe
          (throwError
             err401
               {errReasonPhrase = "You don't have a geopolitik-user cookie"})
          return $
        lookup "geopolitik-user" $ parseCookies cookie
      liftIO $ print token
      maybe
        (throwError err401 {errReasonPhrase = "Could not validate token"})
        return =<<
        runSharedDatabaseT conn (validateToken token)

server :: ServerT GeopolitikAPI M
server = account :<|> article :<|> serveDirectoryWebApp "frontend"

account :: ServerT AccountAPI M
account = signup :<|> signin :<|> newToken

signup :: SignUp -> M (Response SignUp)
signup (SignUp username password) = do
  lookupUsersByUsername [username] >>= \case
    [] -> do
      userCreationDate <- liftIO getCurrentTime
      userID <- liftIO ((Key . toText) <$> randomIO)
      insertUsers [User {..}]
      return SignedUp
    _ -> throwError err403 {errReasonPhrase = "Username already exists"}

signin :: SignIn -> M (H (Response SignIn))
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
                 , setCookiePath = Just "/"
                 , setCookieValue = encodeUtf8 sessionToken
                 }) $
            addHeader "http://localhost:8080" $
            addHeader "Accept" $
            addHeader True $ SignedIn sessionToken
        else throwError err403 {errReasonPhrase = "Wrong password"}
    _ -> throwError err403 {errReasonPhrase = "Wrong username"}

newToken :: User -> M (H (Response SignIn))
newToken User {..} = do
  signin $ SignIn username password

article :: ServerT ArticleAPI M
article user = newArticle user :<|> draft user

newArticle :: User -> NewArticle -> DatabaseT Handler (Response NewArticle)
newArticle User {userID = articleOwner} (NewArticle articleName) = do
  articleCreationDate <- liftIO getCurrentTime
  articleID <- liftIO ((Key . toText) <$> randomIO)
  insertArticles [Article {..}]
  return (ArticleCreated articleID)

link :: User -> LinkDraft -> DatabaseT Handler (Response LinkDraft)
link u@User {..} (LinkDraft linkDraft (SomeTag tag) linkEntity) = do
  linkAuth u tag linkDraft (obvious tag linkEntity)
  linkID <- liftIO ((Key . toText) <$> randomIO)
  let linkTag = SomeTag tag
  insertLinks [Link {..}]
  return Linked

draft :: User -> ServerT DraftAPI M
draft user =
  newDraft user :<|> comments user :<|> link user :<|> latest user :<|>
  latest' user

comments :: User -> DraftComments -> M (Response DraftComments)
comments _ (DraftComments draftKey) = do
  c <- ask
  fmap DraftCommentsFound $
    liftIO $
    query
      c
      [sql|
     select (comments.id, comments.author, comments.content) from comments
        inner join links on comments.id = links.entity 
        where comments.id in ?
        and links.tag = 'CommentTag';
    |]
      (Only (In [draftKey]))

newDraft :: User -> NewDraft -> M (Response NewDraft)
newDraft u@User {userID = draftAuthor} (NewDraft draftArticle draftContents) = do
  draftCreationDate <- liftIO getCurrentTime
  draftID <- liftIO ((Key . toText) <$> randomIO)
  insertDraft u Draft {..}
  return DraftCreated

latest :: User -> LatestDraft -> M (Response LatestDraft)
latest _ (LatestDraft articleKey) =
  latestDraft articleKey >>= \case
    Just Draft {..} ->
      return $
      LatestDraftFound draftID draftAuthor draftContents draftCreationDate
    Nothing -> return LatestDraftNotFound

latest' :: User -> Text -> Text -> M (Response LatestDraft)
latest' _ username articleName =
  latestDraft' username articleName >>= \case
    Just Draft {..} ->
      return $
      LatestDraftFound draftID draftAuthor draftContents draftCreationDate
    Nothing -> return LatestDraftNotFound
