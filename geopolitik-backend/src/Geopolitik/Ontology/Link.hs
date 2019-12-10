module Geopolitik.Ontology.Link where

import Geopolitik.Ontology.Tag
import Geopolitik.Ontology.Key
import Geopolitik.Ontology.Draft
import GHC.Generics
import Data.Void
import Data.Time.Clock
import Data.Aeson
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.FromRow

data Link = Link
  { linkID :: Key Link
  , linkTag :: SomeTag
  , linkDraft :: Key Draft
  , linkEntity :: Key Void
  , linkCreationDate :: UTCTime }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToRow, FromRow, ToJSON)
