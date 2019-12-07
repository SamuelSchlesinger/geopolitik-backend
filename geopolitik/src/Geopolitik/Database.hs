{-# LANGUAGE UndecidableInstances #-}
module Geopolitik.Database where

import Geopolitik.Ontology
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Control.Monad
import Data.String
import Control.Monad.Trans
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.Reader.Class
import Control.Monad.Catch
import Data.Text (Text)
import Control.Monad.Except
import Control.Applicative

testInfo :: ConnectInfo
testInfo = ConnectInfo { 
      connectHost = "localhost"
    , connectPort = 5432
    , connectUser = "sam"
    , connectPassword = ""
    , connectDatabase = "geopolitik"
    }

runTestDatabaseT :: (MonadIO m, MonadMask m) => DatabaseT m a -> m a
runTestDatabaseT (DatabaseT (ReaderT r)) 
  = bracket (liftIO (connect testInfo)) (liftIO . close) r

runDatabaseT :: (MonadIO m, MonadMask m) => ConnectInfo -> DatabaseT m a -> m a
runDatabaseT i (DatabaseT (ReaderT r)) = bracket (liftIO $ connect i) (liftIO . close) r

runSharedDatabaseT :: (MonadIO m, MonadMask m) => Connection -> DatabaseT m a -> m a
runSharedDatabaseT i (DatabaseT (ReaderT r)) = r i

newtype DatabaseT m a = DatabaseT { unDatabaseT :: ReaderT Connection m a }
  deriving newtype (MonadReader Connection, Monad, Functor, Applicative, MonadIO, MonadTrans, MonadMask, MonadThrow, MonadCatch, Alternative, MonadPlus)

instance (MonadError e m, MonadCatch m, Exception e) => MonadError e (DatabaseT m) where
  throwError = lift . throwError
  catchError a f = catch a f

insertUsers :: MonadIO m => [User] -> DatabaseT m ()
insertUsers users = do
  c <- ask
  void . liftIO $ executeMany c [sql|
    insert into users (id, username, password, creation_date) 
    values (?, ?, ?, ?);
   |] ((\User{..} -> (userID, username, password, userCreationDate)) <$> users) 

insertArticles :: MonadIO m => [Article] -> DatabaseT m ()
insertArticles articles = do
  c <- ask
  void . liftIO $ executeMany c [sql|
    insert into articles (id, name, owner, creation_date)
    values (?, ?, ?, ?);
   |] ((\Article{..} -> 
      (articleID, articleName, articleOwner, articleCreationDate)) 
      <$> articles)

insertDraft :: MonadIO m => User -> Draft -> DatabaseT m ()
insertDraft User{..} Draft{..} = do
  c <- ask
  void . liftIO $ execute c [sql|
    insert into drafts (id, article, author, contents, creation_date)
    values (?, ?, ?, ?, ?);
   |] (draftID, draftArticle, userID, draftContents, draftCreationDate)

insertSessions :: MonadIO m => [Session] -> DatabaseT m ()
insertSessions sessions = do
  c <- ask
  void . liftIO $ executeMany c [sql|
    insert into sessions (id, owner, creation_date, token)
    values (?, ?, ?, ?);
    |] ((\Session{..} -> 
       (sessionID, sessionOwner, sessionCreationDate, sessionToken)) 
       <$> sessions)

insertExecutedMigrations :: MonadIO m => [ExecutedMigration] -> DatabaseT m ()
insertExecutedMigrations executedMigrations = do
  c <- ask
  void . liftIO $ executeMany c [sql|
    insert into executed_migrations (id, file_name)
    values (?, ?);
  |] executedMigrations

insertLinks :: MonadIO m => [Link] -> DatabaseT m ()
insertLinks links = do
  c <- ask
  void . liftIO $ executeMany c [sql|
    insert into links
    values (?, ?, ?, ?);
  |] links

insertCollaborators :: MonadIO m => [Collaborator] -> DatabaseT m ()
insertCollaborators collaborators = do
  c <- ask
  void . liftIO $ executeMany c [sql|
    insert into collaborators (id, article, collaborator, creation_date)
         values (?, ?, ?, ?)
  |] collaborators

-- TODO make not awful
insertLocation :: MonadIO m => Location -> DatabaseT m ()
insertLocation Location{locationSpot = Coordinate x y, ..} = do
  c <- ask
  void . liftIO $ execute c ([sql|
    insert into locations
    values (?, ?, ?, |] <> fromString ("'POINT(" <> show x <> " " <> show y <> ")'") <> [sql|, ?)
  |]) (locationID, locationName, locationDescription, locationCreationDate)

isCollaborator :: MonadIO m => Key User -> Key Article -> DatabaseT m (Maybe Bool)
isCollaborator collaborator article = do
  c <- ask
  liftIO $ query c [sql|
    select * from collaborators where collaborator = ? and article = ?;
  |] (collaborator, article) >>= \case
    [Collaborator{}] -> return (Just True)
    [] ->  return (Just False)
    _ -> return Nothing

validateToken :: MonadIO m => Text -> DatabaseT m (Maybe User)
validateToken token = do
  c <- ask
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
      
lookupUsers :: MonadIO m => [Key User] -> DatabaseT m [User]
lookupUsers users = do
  c <- ask
  liftIO $ query c [sql|
     select * from users where id in ?;
    |] (Only (In users))

lookupUsersByUsername :: MonadIO m => [Text] -> DatabaseT m [User]
lookupUsersByUsername usernames = do
  c <- ask
  liftIO $ query c [sql|
     select * from users where username in ?;
    |] (Only (In usernames))

lookupArticles :: MonadIO m => [Key Article] -> DatabaseT m [Article]
lookupArticles articles = do
  c <- ask
  liftIO $ query c [sql|
     select * from articles where id in ?;
    |] (Only (In articles))

lookupArticleByName :: MonadIO m => User -> Text -> DatabaseT m (Maybe Article)
lookupArticleByName User{..} articleName = do
  c <- ask
  liftIO $ query c [sql|
     select * from articles where owner = ? and name = ?;
    |] (userID, articleName) >>= \case
    [a] -> return (Just a)
    _ -> return Nothing

lookupExecutedMigrations :: MonadIO m => [FilePath] -> DatabaseT m [ExecutedMigration]
lookupExecutedMigrations names = do
  c <- ask
  liftIO $ query c [sql|
    select * from executed_migrations where file_name in ?;
  |] (Only (In names))

lookupDrafts :: MonadIO m => [Key Draft] -> DatabaseT m [Draft]
lookupDrafts drafts = do
  c <- ask
  liftIO $ query c [sql|
     select * from drafts where id in ?;
    |] (Only (In drafts))

lookupComments :: MonadIO m => [Key Comment] -> DatabaseT m [Comment]
lookupComments comments = do
  c <- ask
  liftIO $ query c [sql|
    select * from comments where id in ?;
   |] (Only (In comments))
                                
lookupLocations :: MonadIO m => [Key Location] -> DatabaseT m [Location]
lookupLocations locations = do
  c <- ask
  liftIO $ query c [sql|
    select (id, name, description, ST_AsText(spot), creation_date) from locations where id in ?;
    |] (Only (In locations))

lookupEntities :: MonadIO m => Tag a -> [Key a] -> DatabaseT m [a]
lookupEntities tag ks = case tag of
  ArticleTag -> lookupArticles ks
  UserTag -> lookupUsers ks
  CommentTag -> lookupComments ks
  DraftTag -> lookupDrafts ks 
  LocationTag -> lookupLocations ks
  

latestDraft :: MonadIO m => Key Article -> DatabaseT m (Maybe Draft)
latestDraft article = do
  c <- ask
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

latestDraft' :: MonadIO m => Text -> Text -> DatabaseT m (Maybe Draft)
latestDraft' username articleName = do
  c <- ask
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

deleteUsers :: MonadIO m => [Key User] -> DatabaseT m Int
deleteUsers users = do
  c <- ask
  fmap fromIntegral . liftIO $ execute c [sql|
     delete from users where id in ?;
    |] (Only (In users))

deleteArticles :: MonadIO m => [Key Article] -> DatabaseT m Int
deleteArticles articles = do
  c <- ask
  fmap fromIntegral . liftIO $ execute c [sql|
     delete from articles where id in ?;
    |] (Only (In articles))

deleteDrafts :: MonadIO m => [Key Draft] -> DatabaseT m Int
deleteDrafts drafts = do
  c <- ask
  fmap fromIntegral . liftIO $ execute c [sql|
     delete from drafts where id in ?; 
    |] (Only (In drafts))

withTestUsers :: (MonadIO m, MonadMask m) => [User] -> ([Key User] -> DatabaseT m a) -> DatabaseT m a
withTestUsers users dbGo = bracket (insertUsers users) (const . void $ deleteUsers userKeys) (const $ dbGo userKeys) where
  userKeys = map userID users

withTestArticles :: (MonadIO m, MonadMask m) => [Article] -> ([Key Article] -> DatabaseT m a) -> DatabaseT m a
withTestArticles articles dbGo = bracket (insertArticles articles) (const $ deleteArticles articleKeys) (const $ dbGo articleKeys) where
  articleKeys = map articleID articles
  
