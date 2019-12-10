module Geopolitik.Ontology.Article where

import Geopolitik.Ontology.User
import Geopolitik.Ontology.Key
import Data.Text
import Data.Aeson
import GHC.Generics
import Data.Time.Clock
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow

data Article = Article
  { articleID :: Key Article
  , articleName :: Text
  , articleOwner :: Key User
  , articleCreationDate :: UTCTime
  } deriving stock (Generic, Eq, Show, Read, Ord)
    deriving anyclass (FromRow, ToRow, ToJSON, FromJSON)
