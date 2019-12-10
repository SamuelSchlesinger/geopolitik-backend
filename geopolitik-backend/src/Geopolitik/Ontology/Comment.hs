module Geopolitik.Ontology.Comment where

import Data.Aeson
import Geopolitik.Ontology.Key
import Data.Time.Clock
import Geopolitik.Ontology.User
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.FromRow
import Data.Text
import GHC.Generics

data Comment = Comment
  { commentID :: Key Comment
  , commentAuthor :: Key User
  , commentContent :: Text
  , commentCreationDate :: UTCTime
  } deriving stock (Generic, Eq, Show, Read, Ord)
    deriving anyclass (ToRow, FromRow, ToJSON, FromJSON)
