{-# LANGUAGE UndecidableInstances #-}
module Geopolitik.Database where

import Control.Applicative
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Reader.Class
import Control.Monad.Trans.Reader (ReaderT(..))
import Data.String
import Data.Text (Text)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Geopolitik.Ontology

testInfo :: String -> ConnectInfo
testInfo username = ConnectInfo { 
      connectHost = "localhost"
    , connectPort = 5432
    , connectUser = username
    , connectPassword = ""
    , connectDatabase = "geopolitik"
    }

runTestDatabaseT :: (MonadIO m, MonadMask m) => String -> DatabaseT m a -> m a
runTestDatabaseT username (DatabaseT (ReaderT r)) 
  = bracket (liftIO (connect (testInfo username))) (liftIO . close) r

runDatabaseT :: (MonadIO m, MonadMask m) => ConnectInfo -> DatabaseT m a -> m a
runDatabaseT i (DatabaseT (ReaderT r)) = bracket (liftIO $ connect i) (liftIO . close) r

runSharedDatabaseT :: (MonadIO m, MonadMask m) => Connection -> DatabaseT m a -> m a
runSharedDatabaseT i (DatabaseT (ReaderT r)) = r i

newtype DatabaseT m a = DatabaseT { unDatabaseT :: ReaderT Connection m a }
  deriving newtype (MonadReader Connection, Monad, Functor, Applicative, MonadIO, MonadTrans, MonadMask, MonadThrow, MonadCatch, Alternative, MonadPlus)

instance (MonadError e m, MonadCatch m, Exception e) => MonadError e (DatabaseT m) where
  throwError = lift . throwError
  catchError a f = catch a f

class MonadIO m => MonadDatabase m where
  withConnection :: (Connection -> m a) -> m a

instance MonadIO m => MonadDatabase (DatabaseT m) where
  withConnection = (ask >>=)

insertUsers :: MonadDatabase m => [User] -> m ()
insertUsers users = withConnection \c -> do
  void . liftIO $ executeMany c [sql|
    insert into users (id, username, password, creation_date) 
    values (?, ?, ?, ?);
   |] ((\User{..} -> (userID, username, password, userCreationDate)) <$> users) 

insertArticles :: MonadDatabase m => [Article] -> m ()
insertArticles articles = withConnection \c -> do
  void . liftIO $ executeMany c [sql|
    insert into articles (id, name, owner, creation_date)
    values (?, ?, ?, ?);
   |] ((\Article{..} -> 
      (articleID, articleName, articleOwner, articleCreationDate)) 
      <$> articles)

insertDraft :: MonadDatabase m => User -> Draft -> m ()
insertDraft User{..} Draft{..} = withConnection \c -> do
  void . liftIO $ execute c [sql|
    insert into drafts (id, article, author, contents, creation_date)
    values (?, ?, ?, ?, ?);
   |] (draftID, draftArticle, userID, draftContents, draftCreationDate)

insertSessions :: MonadDatabase m => [Session] -> m ()
insertSessions sessions = withConnection \c -> do
  void . liftIO $ executeMany c [sql|
    insert into sessions (id, owner, creation_date, token)
    values (?, ?, ?, ?);
    |] ((\Session{..} -> 
       (sessionID, sessionOwner, sessionCreationDate, sessionToken)) 
       <$> sessions)

insertExecutedMigrations :: MonadDatabase m => [ExecutedMigration] -> m ()
insertExecutedMigrations executedMigrations = withConnection \c -> do
  void . liftIO $ executeMany c [sql|
    insert into executed_migrations (id, file_name)
    values (?, ?);
  |] executedMigrations

insertLinks :: MonadDatabase m => [Link] -> m ()
insertLinks links = withConnection \c -> do
  void . liftIO $ executeMany c [sql|
    insert into links
    values (?, ?, ?, ?);
  |] links

insertCollaborators :: MonadDatabase m => [Collaborator] -> m ()
insertCollaborators collaborators = withConnection \c -> do
  void . liftIO $ executeMany c [sql|
    insert into collaborators (id, article, collaborator, creation_date)
         values (?, ?, ?, ?)
  |] collaborators

-- TODO make not awful
insertLocation :: MonadDatabase m => Location -> m ()
insertLocation Location{locationSpot = Coordinate x y, ..} = withConnection \c -> do
  void . liftIO $ execute c ([sql|
    insert into locations
    values (?, ?, ?, |] <> fromString ("'POINT(" <> show x <> " " <> show y <> ")'") <> [sql|, ?)
  |]) (locationID, locationName, locationDescription, locationCreationDate)

isCollaborator :: MonadDatabase m => Key User -> Key Article -> m (Maybe Bool)
isCollaborator collaborator article = withConnection \c -> do
  liftIO $ query c [sql|
    select * from collaborators where collaborator = ? and article = ?;
  |] (collaborator, article) >>= \case
    [Collaborator{}] -> return (Just True)
    [] ->  return (Just False)
    _ -> return Nothing

validateToken :: MonadDatabase m => Text -> m (Maybe User)
validateToken token = withConnection \c -> do
  (liftIO $ query c [sql|
    select users.id, username, password, users.creation_date 
    from users
    inner join sessions on owner=users.id
                        and token in ?
                      where sessions.creation_date >= now() - interval '1 hour';
    |] (Only (In [token]))) >>= \case
      [] -> do
        return Nothing
      [s] -> do
        return (Just s)
      _ -> error "database has two sessions with the same token" 
      
lookupUsers :: MonadDatabase m => [Key User] -> m [User]
lookupUsers users = withConnection \c -> do
  liftIO $ query c [sql|
     select * from users where id in ?;
    |] (Only (In users))

lookupUsersByUsername :: MonadDatabase m => [Text] -> m [User]
lookupUsersByUsername usernames = withConnection \c -> do
  liftIO $ query c [sql|
     select * from users where username in ?;
    |] (Only (In usernames))

lookupArticles :: MonadDatabase m => [Key Article] -> m [Article]
lookupArticles articles = withConnection \c -> do
  liftIO $ query c [sql|
     select * from articles where id in ?;
    |] (Only (In articles))

lookupArticleByName :: MonadDatabase m => User -> Text -> m (Maybe Article)
lookupArticleByName User{..} articleName = withConnection \c -> do
  liftIO $ query c [sql|
     select * from articles where owner = ? and name = ?;
    |] (userID, articleName) >>= \case
    [a] -> return (Just a)
    _ -> return Nothing

lookupExecutedMigrations :: MonadDatabase m => [FilePath] -> m [ExecutedMigration]
lookupExecutedMigrations names = withConnection \c -> do
  liftIO $ query c [sql|
    select * from executed_migrations where file_name in ?;
  |] (Only (In names))

lookupDrafts :: MonadDatabase m => [Key Draft] -> m [Draft]
lookupDrafts drafts = withConnection \c -> do
  liftIO $ query c [sql|
     select * from drafts where id in ?;
    |] (Only (In drafts))

lookupComments :: MonadDatabase m => [Key Comment] -> m [Comment]
lookupComments comments = withConnection \c -> do
  liftIO $ query c [sql|
    select * from comments where id in ?;
   |] (Only (In comments))
                                
lookupLocations :: MonadDatabase m => [Key Location] -> m [Location]
lookupLocations locations = withConnection \c -> do
  liftIO $ query c [sql|
    select (id, name, description, ST_AsText(spot), creation_date) from locations where id in ?;
    |] (Only (In locations))

lookupEntities :: MonadDatabase m => Tag a -> [Key a] -> m [a]
lookupEntities tag ks = case tag of
  ArticleTag -> lookupArticles ks
  UserTag -> lookupUsers ks
  CommentTag -> lookupComments ks
  DraftTag -> lookupDrafts ks 
  LocationTag -> lookupLocations ks
  

latestDraft :: MonadDatabase m => Key Article -> m (Maybe Draft)
latestDraft article = withConnection \c -> do
  x <- liftIO $ query c [sql|
     select * from drafts
     where article in ?
           order by creation_date desc
           limit 1;
    |] (Only (In [article]))
  case x of
    [a] -> return (Just a)
    [] -> return Nothing
    _ -> error "unacceptable state"

latestDraft' :: MonadDatabase m => Text -> Text -> m (Maybe Draft)
latestDraft' username articleName = withConnection \c -> do
  x <- liftIO $ query c [sql|
     select drafts.id, drafts.article, drafts.author, drafts.contents, drafts.creation_date from drafts
     inner join articles on articles.id = drafts.article
     inner join users on users.id = articles.owner
     where users.username = ?
       and articles.name = ?
           order by drafts.creation_date desc
           limit 1;
    |] (username, articleName)
  case x of
    [a] -> return (Just a)
    [] -> return Nothing
    _ -> error "unacceptable state'"

deleteUsers :: MonadDatabase m => [Key User] -> m Int
deleteUsers users = withConnection \c -> do
  fmap fromIntegral . liftIO $ execute c [sql|
     delete from users where id in ?;
    |] (Only (In users))

deleteArticles :: MonadDatabase m => [Key Article] -> m Int
deleteArticles articles = withConnection \c -> do
  fmap fromIntegral . liftIO $ execute c [sql|
     delete from articles where id in ?;
    |] (Only (In articles))

deleteDrafts :: MonadDatabase m => [Key Draft] -> m Int
deleteDrafts drafts = withConnection \c -> do
  fmap fromIntegral . liftIO $ execute c [sql|
     delete from drafts where id in ?; 
    |] (Only (In drafts))

withTestUsers :: (MonadDatabase m, MonadMask m) => [User] -> ([Key User] -> m a) -> m a
withTestUsers users dbGo = bracket (insertUsers users) (const . void $ deleteUsers userKeys) (const $ dbGo userKeys) where
  userKeys = map userID users

withTestArticles :: (MonadDatabase m, MonadMask m) => [Article] -> ([Key Article] -> m a) -> m a
withTestArticles articles dbGo = bracket (insertArticles articles) (const $ deleteArticles articleKeys) (const $ dbGo articleKeys) where
  articleKeys = map articleID articles
  
