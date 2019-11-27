module Main where

import Geopolitik.Ontology
import Geopolitik.Database
import Data.Text (pack)
import Data.Time.Clock (getCurrentTime)

main :: IO ()
main = do
  userCreationDate <- getCurrentTime
  let ids = [ Key (pack (show i)) | (i :: Int) <- [1..100]]
  runDatabaseT $ insertUsers do
    userID <- ids
    let username = getKey userID
    let password = "password"
    return User{..} 
  print =<< (runDatabaseT $ lookupUsers ids)
