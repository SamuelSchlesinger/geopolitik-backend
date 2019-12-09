module Main where

import Control.Monad
import Control.Monad.Reader.Class
import Control.Monad.Trans
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Geopolitik.Database
import System.Posix.User

main :: IO ()
main = do
  username <- getEffectiveUserName
  runDatabaseT (testInfo username) do
    c <- ask
    void . liftIO $ execute_ c [sql|
     drop table executed_migrations cascade;
     drop table users cascade;
     drop table articles cascade;
     drop table drafts cascade;
     drop table sessions;
     drop table links;
     drop table locations;
     drop table comments;
     drop table collaborators;
     drop extension postgis;
    |]

