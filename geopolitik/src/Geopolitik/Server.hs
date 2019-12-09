module Geopolitik.Server (geopolitikProxy, ctx, ctxProxy, server) where

import Control.Monad.Except
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
import Geopolitik.Monad
import Servant
import Servant.Server.Experimental.Auth
import System.Random
import Web.Cookie
import qualified Network.Wai as Wai

geopolitikProxy :: Proxy GeopolitikAPI
geopolitikProxy = Proxy

ctxProxy :: Proxy Ctx
ctxProxy = Proxy

ctx :: Connection -> Context Ctx
ctx conn = mkAuthHandler validate :. EmptyContext
  where
    validate :: Wai.Request -> Handler User
    validate req = do
      cookie <- maybe
          (throwError err401 {errReasonPhrase = "You have no cookies"})
          return $ lookup "cookie" (Wai.requestHeaders req)
      token <- maybe
          (throwError err401 {errReasonPhrase = "You don't have a geopolitik-user cookie"})
          return $ lookup "geopolitik-user" $ parseCookiesText cookie
      maybe
        (throwError err401 {errReasonPhrase = "Could not validate token"})
        return =<< runSharedDatabaseT conn (validateToken token)

server :: MonadGeopolitik m => ServerT GeopolitikAPI m
server = account :<|> article :<|> serveDirectoryWebApp "static"

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

article :: MonadGeopolitik m => ServerT ArticleAPI m
article user = newArticle user :<|> draft user :<|> collaborator user

newArticle :: MonadGeopolitik m => User -> NewArticle -> m (Response NewArticle)
newArticle User {userID = articleOwner} (NewArticle articleName) = do
  articleCreationDate <- liftIO getCurrentTime
  articleID <- liftIO ((Key . toText) <$> randomIO)
  insertArticles [Article {..}]
  collaboratorID <- liftIO ((Key . toText) <$> randomIO)
  insertCollaborators [Collaborator collaboratorID articleID articleOwner articleCreationDate]
  return (ArticleCreated articleID)

link :: MonadGeopolitik m => User -> LinkDraft -> m (Response LinkDraft)
link u@User {..} (LinkDraft linkDraft (SomeTag tag) linkEntity) = do
  linkAuth u tag linkDraft (obvious tag linkEntity)
  linkCreationDate <- liftIO getCurrentTime
  linkID <- liftIO ((Key . toText) <$> randomIO)
  let linkTag = SomeTag tag
  insertLinks [Link {..}]
  return Linked

draft :: MonadGeopolitik m => User -> ServerT DraftAPI m
draft user =
  newDraft user :<|> comments user :<|> link user :<|> latest user :<|>
  latest' user

comments :: MonadGeopolitik m => User -> DraftComments -> m (Response DraftComments)
comments _ (DraftComments draftKey) = withConnection \c -> do
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

newDraft :: MonadGeopolitik m => User -> NewDraft -> m (Response NewDraft)
newDraft u@User {userID = draftAuthor} (NewDraft draftArticle draftContents) = do
  draftAuth u draftArticle
  draftCreationDate <- liftIO getCurrentTime
  draftID <- liftIO ((Key . toText) <$> randomIO)
  insertDraft u Draft {..}
  return DraftCreated

latest :: MonadGeopolitik m => User -> LatestDraft -> m (Response LatestDraft)
latest _ (LatestDraft articleKey) =
  latestDraft articleKey >>= \case
    Just Draft {..} -> return $ LatestDraftFound draftID draftAuthor draftContents draftCreationDate
    Nothing -> throwError err404
      { errReasonPhrase = "Latest draft not found" }

latest' :: MonadGeopolitik m => User -> Text -> Text -> m (Response LatestDraft)
latest' _ username articleName =
  latestDraft' username articleName >>= \case
    Just Draft {..} ->
      return $
      LatestDraftFound draftID draftAuthor draftContents draftCreationDate
    Nothing -> throwError err404
      { errReasonPhrase = "Latest draft not found" }

collaborator :: MonadGeopolitik m => User -> ServerT CollaboratorAPI m
collaborator user = addCollaborator user

addCollaborator :: MonadGeopolitik m => User -> AddCollaborator -> m (Response AddCollaborator)
addCollaborator User{..} AddCollaborator{..} = do
  collaboratorAuth userID addCollaboratorArticle
  lookupUsersByUsername [addCollaboratorUsername] >>= \case
    [User collaboratorUserID _ _ _ ] -> do
      collaboratorID <- liftIO ((Key . toText) <$> randomIO)
      creationDate <- liftIO getCurrentTime
      insertCollaborators [Collaborator collaboratorID addCollaboratorArticle collaboratorUserID creationDate ]
      return AddedCollaborator
    [] -> throwError err404
      { errReasonPhrase = "User does not exist" }
    _ -> throwError err500
      { errReasonPhrase = "Database is in an invalid state: addCollaborator" }
