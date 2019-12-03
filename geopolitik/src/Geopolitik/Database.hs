{-# LANGUAGE UndecidableInstances #-}
module Geopolitik.Database where

import Data.Time.Clock
import Data.ByteString (ByteString)
import Geopolitik.Ontology
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.Reader.Class
import Control.Monad.Catch
import Data.Text (Text)
import Control.Monad.Except

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
  deriving newtype (MonadReader Connection, Monad, Functor, Applicative, MonadIO, MonadTrans, MonadMask, MonadThrow, MonadCatch)

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
    insert into articles (id, name, author, creation_date)
    values (?, ?, ?, ?);
   |] ((\Article{..} -> 
      (articleID, articleName, articleOwner, articleCreationDate)) 
      <$> articles)

insertDrafts :: MonadIO m => [Draft] -> DatabaseT m ()
insertDrafts drafts = do
  c <- ask
  void . liftIO $ executeMany c [sql|
    insert into drafts (id, article, contents, creation_date)
    values (?, ?, ?, ?);
   |] ((\Draft{..} -> 
      (draftID, draftArticle, draftContents, draftCreationDate)) 
      <$> drafts)

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
  |] ((\ExecutedMigration{..} ->
     (executedMigrationFilePath, executedMigrationTimestamp))
     <$> executedMigrations)

validateToken :: MonadIO m => ByteString -> DatabaseT m (Maybe User)
validateToken token = do
  c <- ask
  present <- liftIO getCurrentTime
  let past = addUTCTime (secondsToNominalDiffTime (-30 * 60)) present
  liftIO $ print token
  (liftIO $ query c [sql|
    select * 
    from users
    inner join sessions on sessions.owner=users.userID
                        and sessions.token=?
    where sessions.creation_time between ? and ?
    |] (token, past, present)) >>= \case
      [] -> return Nothing
      [s] -> return (Just s)
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

withTestDrafts :: (MonadIO m, MonadMask m) => [Draft] -> ([Key Draft] -> DatabaseT m a) -> DatabaseT m a
withTestDrafts drafts dbGo = bracket (insertDrafts drafts)  (const $ deleteDrafts draftKeys) (const $ dbGo draftKeys) where
  draftKeys = map draftID drafts 
