module Geopolitik.Database where

import Geopolitik.Ontology
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.Reader.Class
import Control.Exception

runDatabaseT :: DatabaseT IO a -> IO a
runDatabaseT (DatabaseT (ReaderT r)) = bracket getConnection close r where
  getConnection = connect ConnectInfo {
      connectHost = "localhost"
    , connectPort = 5432
    , connectUser = "sam"
    , connectPassword = ""
    , connectDatabase = "geopolitik"
    }

newtype DatabaseT m a = DatabaseT { unDatabaseT :: ReaderT Connection m a }
  deriving newtype (MonadReader Connection, Monad, Functor, Applicative, MonadIO, MonadTrans)

insertUsers :: MonadIO m => [User] -> DatabaseT m ()
insertUsers users = do
  c <- ask
  void . liftIO $ executeMany c [sql|
    insert into users (id, username, password, creation_date) 
    values (?, ?, ?, ?)
   |] ((\User{..} -> (userID, username, password, userCreationDate)) <$> users) 

insertArticles :: MonadIO m => [Article] -> DatabaseT m ()
insertArticles articles = do
  c <- ask
  void . liftIO $ executeMany c [sql|
    insert into articles (id, name, author, creation_date)
    values (?, ?, ?, ?)
   |] ((\Article{..} -> (articleID, articleName, articleAuthor, articleCreationDate)) <$> articles)

insertDrafts :: MonadIO m => [Draft] -> DatabaseT m ()
insertDrafts drafts = do
  c <- ask
  void . liftIO $ executeMany c [sql|
    insert into drafts (id, article, contents, creation_date)
    values (?, ?, ?, ?)
   |] ((\Draft{..} -> (draftID, draftArticle, draftContents, draftCreationDate)) <$> drafts)

lookupUsers :: MonadIO m => [Key User] -> DatabaseT m [User]
lookupUsers users = do
  c <- ask
  liftIO $ query c [sql|
     select * from users where id in ?
    |] (Only (In users))

lookupArticles :: MonadIO m => [Key Article] -> DatabaseT m [Article]
lookupArticles articles = do
  c <- ask
  liftIO $ query c [sql|
     select * from articles where id in ?
    |] (Only (In articles))

lookupDrafts :: MonadIO m => [Key Draft] -> DatabaseT m [Article]
lookupDrafts drafts = do
  c <- ask
  liftIO $ query c [sql|
     select * from drafts where id in ?
    |] (Only (In drafts))
