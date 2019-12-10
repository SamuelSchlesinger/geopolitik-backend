module Main where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.Reader.Class
import Control.Monad.Trans
import Data.List
import Data.String
import Data.Time.Clock
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Geopolitik.Database
import Geopolitik.Ontology
import System.Environment
import System.Posix.User

main :: IO ()
main = do
  username <- getEffectiveUserName
  runDatabaseT (testInfo username) do
    geopolitikLocation <- liftIO $ getEnv "GEOPOLITIK_LOCATION"
    migrations <- lines <$> liftIO (readFile (geopolitikLocation <> "/migrations/manifest"))
    executedMigrations <- catch (lookupExecutedMigrations migrations) (\(_ :: SomeException) -> return [])
    let migrationsToRun = migrations \\ map executedMigrationFilePath executedMigrations
    forM_ migrationsToRun runMigration

runMigration :: FilePath -> DatabaseT IO ()
runMigration filepath = do
  liftIO $ putStrLn $ "Running migration " <> filepath
  fileLocation <- 
    (<> "/migrations/" <> filepath) 
      <$> liftIO (getEnv "GEOPOLITIK_LOCATION")
  q :: Query <- fromString <$> liftIO (readFile fileLocation)
  c <- ask
  void $ liftIO (execute_ c q)
  currentTime <- liftIO getCurrentTime
  void $ liftIO (execute c [sql|
      insert into executed_migrations (file_name, creation_date)
      values (?, ?);
    |] (filepath, currentTime))
