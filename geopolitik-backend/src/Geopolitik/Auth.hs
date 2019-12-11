module Geopolitik.Auth (linkAuth, draftAuth, collaboratorAuth) where

import Control.Monad.Except
import Data.Text (unpack)
import Geopolitik.Database
import Geopolitik.Monad
import Geopolitik.Ontology
import Servant

exists :: MonadGeopolitik m => Tag a -> Key a -> m ()
exists tag k = lookupEntities tag [k] >>= \case 
  [_] -> return ()
  [] -> throwError err404
    { errReasonPhrase = "Entity does not exist" }
  _ -> throwError err500
    { errReasonPhrase = "Invalid database state: exists " <> show tag }

linkAuth :: MonadGeopolitik m => User -> Tag a -> Key Draft -> Key a -> m () 
linkAuth User{..} tag draftKey entity 
  = exists tag entity >> case tag of
      ArticleTag -> draftAuthorCheck "Link Article" userID draftKey
      UserTag -> draftAuthorCheck "Link User" userID draftKey 
      CommentTag -> draftExistsCheck "Link Comment" draftKey
      LocationTag -> draftAuthorCheck "Link Location" userID draftKey
      DraftTag -> draftAuthorCheck "Link Draft" userID draftKey

draftAuthorCheck 
  :: MonadGeopolitik m 
       => String 
       -> Key User 
       -> Key Draft 
       -> m ()
draftAuthorCheck reasonPrefix userID draftKey = lookupDrafts [draftKey] >>= \case
  [ Draft{..} ] -> 
    if draftAuthor == userID 
      then return () 
      else throwError err403 
        { errReasonPhrase = reasonPrefix 
                         <> ": You are not the author of draft with key " 
                         <> unpack (getKey draftKey) }
  _       -> throwError err404 
        { errReasonPhrase = reasonPrefix 
                         <> ": Could not find draft with key " 
                         <> unpack (getKey draftKey) }

draftExistsCheck :: MonadGeopolitik m => String -> Key Draft -> m ()
draftExistsCheck reasonPrefix draftKey = lookupDrafts [draftKey] >>= \case
  [ Draft{..} ] -> return ()
  _ -> throwError err404 
    { errReasonPhrase = reasonPrefix 
                     <> ": Could not find draft with key " 
                     <> unpack (getKey draftKey) }

draftAuth :: MonadGeopolitik m => User -> Key Article -> m ()
draftAuth User{..} article = lookupArticles [article] >>= \case
  [ Article{..} ] ->  
    isCollaborator' userID article >>= \case
      Just True -> return ()
      Just False -> throwError err403
        { errReasonPhrase = "User is not a collaborator on this article" }
      Nothing -> throwError err500
        { errReasonPhrase = "Invalid database state: draftAuth (A)" }
  [] -> throwError err404 
    { errReasonPhrase = "Article does not exist" }
  _ -> throwError err500
    { errReasonPhrase = "Invalid database state: draftAuth (B)" }

collaboratorAuth :: MonadGeopolitik m => Key User -> Key Article -> m ()
collaboratorAuth user article = lookupArticles [article] >>= \case
  [ Article{..} ] -> 
    if articleOwner == user 
      then return ()
      else throwError err403
        { errReasonPhrase = "User is not the owner of this article" }
  [] -> throwError err404
    { errReasonPhrase = "Article does not exist" }
  _ -> throwError err500
    { errReasonPhrase = "Invalid database state: collaboratorAuth" } 
