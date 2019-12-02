module Main where

import Geopolitik.Ontology
import Geopolitik.Database
import Data.Text (pack)
import Data.Time.Clock (getCurrentTime)
import Control.Monad.Trans

main :: IO ()
main = do
  currentTime <- getCurrentTime
  let ids = [ Key (pack (show i)) | (i :: Int) <- [1..100]]
  let users = [ User{..} | userID <- ids, let username = getKey userID, let password = "password", let userCreationDate = currentTime ]
  let articles = [ Article (Key (getKey userID)) (getKey userID <> "'s very first article") userID currentTime  | User{..} <- users ]
  let drafts = [ Draft (Key (getKey articleID <> "_1")) articleID "Hello, world" currentTime | Article{..} <- articles ]
  runTestDatabaseT $ 
    withTestUsers users \userKeys ->
    withTestArticles articles \articleKeys ->
    withTestDrafts drafts \draftKeys -> do
      let user = head userKeys
      let article = head articleKeys
      let draft = head draftKeys
      lookupUsers [user] >>= (liftIO . print)
      lookupArticles [article] >>= (liftIO . print)
      lookupDrafts [draft] >>= (liftIO . print)
      latestDraft article >>= (liftIO . print)
