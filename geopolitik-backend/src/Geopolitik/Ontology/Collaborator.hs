module Geopolitik.Ontology.Collaborator where

import Data.Aeson
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Data.Time.Clock
import GHC.Generics
import Geopolitik.Ontology.Key
import Geopolitik.Ontology.User
import Geopolitik.Ontology.Article

data Collaborator = Collaborator
  { collaboratorID :: Key Collaborator
  , collaboratorArticle :: Key Article
  , collaboratorUser :: Key User
  , collaboratorCreationDate :: UTCTime
  } deriving stock (Generic, Eq, Show, Read, Ord)
    deriving anyclass (ToRow, FromRow, ToJSON, FromJSON)
