module Geopolitik.Auth.IsOwner where

import Geopolitik.Auth.Named
import Geopolitik.Ontology
import Geopolitik.Database
import Geopolitik.Monad
import Control.Monad.Except
import Servant

data IsOwner owner article = IsOwner

isOwner :: MonadGeopolitik m => Named owner (Key User) -> Named article (Key Article) -> m (IsOwner owner article)
isOwner (anon -> user) (anon -> article) = lookupArticles [article] >>= \case
  [ Article{..} ] -> if articleOwner == user then return IsOwner else throwError err403
  [] -> throwError err404
  _ -> throwError err500
