module Geopolitik.Auth.Exists 
  ( Exists
  , exists
  ) where

import Control.Monad.Except
import Geopolitik.Database
import Geopolitik.Ontology
import Geopolitik.Monad
import Servant
import Geopolitik.Auth.Named

data Exists name = Exists

exists :: MonadGeopolitik m => Tag a -> Named name (Key a) -> m (Exists name)
exists tag (anon -> k) = lookupEntities tag [k] >>= \case
  [_] -> return Exists
  [] -> throwError err404
    { errReasonPhrase = "Entity does not exist" }
  _ -> throwError err500
    { errReasonPhrase = "Invalid database state: exists " <> show tag }
