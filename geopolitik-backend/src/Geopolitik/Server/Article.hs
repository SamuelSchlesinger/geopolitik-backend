module Geopolitik.Server.Article (article) where

import Control.Monad.Except
import Data.Text (Text)
import Data.Time.Clock
import Data.UUID
import Database.PostgreSQL.Simple hiding ((:.))
import Database.PostgreSQL.Simple.SqlQQ
import Geopolitik.API
import Geopolitik.Auth
import Geopolitik.Database
import Geopolitik.Ontology.User
import Geopolitik.Ontology.Article
import Geopolitik.Ontology.Tag
import Geopolitik.Ontology.Link
import Geopolitik.Ontology.Draft
import Geopolitik.Ontology.Key
import Geopolitik.Ontology.Collaborator
import Geopolitik.Monad
import Servant
import System.Random

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
