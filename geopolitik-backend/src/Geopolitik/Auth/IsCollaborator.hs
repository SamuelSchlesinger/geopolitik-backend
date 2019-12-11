module Geopolitik.Auth.IsCollaborator 
  ( IsCollaborator
  , isCollaborator
  ) where

import Geopolitik.Auth.Named
import Geopolitik.Ontology
import Geopolitik.Monad
import Geopolitik.Database
import Servant

data IsCollaborator collaborator article = IsCollaborator

isCollaborator :: MonadGeopolitik m => Named collaborator (Key User) -> Named article (Key Article) -> m (IsCollaborator collaborator article)
isCollaborator (anon -> user) (anon -> article) = isCollaborator' user article >>= \case
  Just True -> return IsCollaborator
  Just False -> throwError err403
  Nothing -> throwError err404
