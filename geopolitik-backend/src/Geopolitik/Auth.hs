module Geopolitik.Auth (draftAuth, collaboratorAuth, linkable, Linkable(..)) where

import Control.Monad.Except
import Geopolitik.Database
import Geopolitik.Monad
import Geopolitik.Ontology
import Servant
import Geopolitik.Auth.Named
import Geopolitik.Auth.CanLink
import Geopolitik.Auth.IsOwner

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
collaboratorAuth user article = named user \u -> named article \a -> () <$ isOwner u a
