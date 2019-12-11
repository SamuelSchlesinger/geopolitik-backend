module Geopolitik.Auth.IsAuthor 
  ( IsAuthor
  , isAuthor
  ) where

import Geopolitik.Ontology
import Geopolitik.Auth.Named
import Servant
import Geopolitik.Monad
import Geopolitik.Database

data IsAuthor author draft = IsAuthor 

isAuthor :: MonadGeopolitik m => Named author User -> Named draft (Key Draft) -> m (IsAuthor author draft)
isAuthor (anon -> User{..}) (anon -> draftKey) = lookupDrafts [draftKey] >>= \case
  [ Draft{..} ] ->
    if draftAuthor == userID
      then return IsAuthor
      else throwError err403
  _       -> throwError err404
