module Geopolitik.Auth.CanLink
  ( CanLink
  , canLink
  , Linkable(..)
  , linkable
  ) where

import Geopolitik.Auth.Exists
import Geopolitik.Monad
import Geopolitik.Auth.Named
import Geopolitik.Ontology
import Geopolitik.Auth.IsAuthor

data CanLink user draft entity = CanLink 

data Linkable = forall user draft entity a. Linkable (Named user User) (Tag a) (Named draft (Key Draft)) (Named entity (Key a)) (CanLink user draft entity)

canLink :: MonadGeopolitik m => Named user User -> Tag a -> Named draft (Key Draft) -> Named entity (Key a) -> m (CanLink user draft entity)
canLink user tag draftKey entity
  = exists tag entity >> case tag of
      ArticleTag  -> CanLink <$ isAuthor user draftKey
      UserTag     -> CanLink <$ isAuthor user draftKey
      CommentTag  -> CanLink <$ exists DraftTag draftKey
      LocationTag -> CanLink <$ isAuthor user draftKey
      DraftTag    -> CanLink <$ isAuthor user draftKey

linkable :: MonadGeopolitik m => User -> Tag a -> Key Draft -> Key a -> m Linkable
linkable user tag draftKey entity 
  = named user \u ->
    named draftKey \d ->
    named entity \e -> do
      pf <- canLink u tag d e
      return (Linkable u tag d e pf)
