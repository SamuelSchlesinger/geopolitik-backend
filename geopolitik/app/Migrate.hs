module Main where

import Data.String
import Geopolitik.Database
import Geopolitik.Ontology
import Data.List
import System.Environment
import Control.Monad
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Control.Monad.Trans
import Control.Monad.Reader.Class
import Data.Time.Clock
import Control.Monad.Catch

main :: IO ()
main = runDatabaseT testInfo do
  geopolitikLocation <- liftIO $ getEnv "GEOPOLITIK_LOCATION"
  migrations <- lines <$> liftIO (readFile (geopolitikLocation <> "/geopolitik/migrations/manifest"))
  executedMigrations <- catch (lookupExecutedMigrations migrations) (\(_ :: SomeException) -> return [])
  let migrationsToRun = migrations \\ map executedMigrationFilePath executedMigrations
  forM_ migrationsToRun runMigration

runMigration :: FilePath -> DatabaseT IO ()
runMigration filepath = do
  fileLocation <- 
    (<> "/geopolitik/migrations/" <> filepath) 
      <$> liftIO (getEnv "GEOPOLITIK_LOCATION")
  q :: Query <- fromString <$> liftIO (readFile fileLocation)
  c <- ask
  void $ liftIO (execute_ c q)
  currentTime <- liftIO getCurrentTime
  void $ liftIO (execute c [sql|
      insert into executed_migrations (file_name, creation_date)
      values (?, ?);
    |] (filepath, currentTime))
