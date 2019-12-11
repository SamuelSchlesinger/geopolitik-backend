module Geopolitik.Auth.CanLink
  ( CanLink
  , canLink
  ) where

import Geopolitik.Auth.Exists
import Geopolitik.Monad
import Geopolitik.Auth.Named
import Geopolitik.Ontology
import Geopolitik.Auth.IsAuthor

data CanLink user draft entity = CanLink

canLink :: MonadGeopolitik m => Named user User -> Tag a -> Named draft (Key Draft) -> Named entity (Key a) -> m (CanLink user draft entity)
canLink user tag draftKey entity
  = exists tag entity >> case tag of
      ArticleTag  -> CanLink <$ isAuthor user draftKey
      UserTag     -> CanLink <$ isAuthor user draftKey
      CommentTag  -> CanLink <$ exists DraftTag draftKey
      LocationTag -> CanLink <$ isAuthor user draftKey
      DraftTag    -> CanLink <$ isAuthor user draftKey
