module Geopolitik.Auth.Exists 
  ( Exists
  , Existent(..)
  , exists
  , existent
  ) where

import Control.Monad.Except
import Geopolitik.Database
import Geopolitik.Ontology
import Geopolitik.Monad
import Servant
import Geopolitik.Auth.Named

data Exists name = Exists

data Existent a = forall name. Existent (Exists name) (Named name (Key a))

exists :: MonadGeopolitik m => Tag a -> Named name (Key a) -> m (Exists name)
exists tag (anon -> k) = lookupEntities tag [k] >>= \case
  [_] -> return Exists
  [] -> throwError err404
    { errReasonPhrase = "Entity does not exist" }
  _ -> throwError err500
    { errReasonPhrase = "Invalid database state: exists " <> show tag }

existent :: MonadGeopolitik m => Tag a -> Key a -> m (Existent a)
existent tag entity = named entity \k -> do
  pf <- exists tag k
  return (Existent pf k)
