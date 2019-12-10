module Geopolitik.Ontology.Draft where

import Geopolitik.Ontology.Article
import Geopolitik.Ontology.Key
import Geopolitik.Ontology.User
import Data.Time.Clock
import Data.Text
import GHC.Generics
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.FromRow
import Data.Aeson

data Draft = Draft
  { draftID :: Key Draft
  , draftArticle :: Key Article
  , draftAuthor :: Key User
  , draftContents :: Text
  , draftCreationDate :: UTCTime
  } deriving stock (Generic, Eq, Show, Read, Ord)
    deriving anyclass (FromRow, ToRow, ToJSON, FromJSON)
