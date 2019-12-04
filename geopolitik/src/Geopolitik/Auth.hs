module Geopolitik.Auth where

import Geopolitik.Ontology
import Geopolitik.Database
import Control.Monad.Except
import Servant
import Data.Text (unpack)
import Control.Monad.Catch

exists :: (MonadIO m, MonadError ServerError m, MonadCatch m) => Tag a -> Key a -> DatabaseT m ()
exists tag k = lookupEntities tag [k] >>= \case { [_] -> return (); _ -> throwError err404 }

linkAuth :: (MonadCatch m, MonadError ServerError m, MonadIO m) => User -> Tag a -> Key Draft -> Key a -> DatabaseT m () 
linkAuth User{..} tag draftKey entity 
  = exists tag entity >> case tag of
      ArticleTag -> draftAuthorCheck "Link Article" userID draftKey
      UserTag -> draftAuthorCheck "Link User" userID draftKey 
      CommentTag -> draftExistsCheck "Link Comment" draftKey
      LocationTag -> draftAuthorCheck "Link Location" userID draftKey
      DraftTag -> draftAuthorCheck "Link Draft" userID draftKey

draftAuthorCheck :: (MonadIO m, MonadError ServerError m, MonadCatch m) => String -> Key User -> Key Draft -> DatabaseT m ()
draftAuthorCheck reasonPrefix userID draftKey = lookupDrafts [draftKey] >>= \case
  [ Draft{..} ] -> if draftAuthor == userID then return () else throwError err403 { errReasonPhrase = reasonPrefix <> ": You are not the author of draft with key " <> unpack (getKey draftKey) }
  _       -> throwError err404 { errReasonPhrase = reasonPrefix <> ": Could not find draft with key " <> unpack (getKey draftKey) }

draftExistsCheck :: (MonadIO m, MonadError ServerError m, MonadCatch m) => String -> Key Draft -> DatabaseT m ()
draftExistsCheck reasonPrefix draftKey = lookupDrafts [draftKey] >>= \case
  [ Draft{..} ] -> return ()
  _ -> throwError err404 { errReasonPhrase = reasonPrefix <> ": Could not find draft with key " <> unpack (getKey draftKey) }
