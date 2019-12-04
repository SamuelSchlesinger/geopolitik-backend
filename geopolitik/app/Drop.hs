module Main where
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Geopolitik.Database
import Control.Monad.Trans
import Control.Monad
import Control.Monad.Reader.Class

main :: IO ()
main = runDatabaseT testInfo do
  c <- ask
  void . liftIO $ execute_ c [sql|
     drop table executed_migrations cascade;
     drop table users cascade;
     drop table articles cascade;
     drop table drafts cascade;
     drop table sessions;
     drop table links;
     drop table locations;
     drop extension postgis;
    |]

