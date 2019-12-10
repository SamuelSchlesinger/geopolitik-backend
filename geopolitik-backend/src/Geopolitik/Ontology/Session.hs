module Geopolitik.Ontology.Session where

import Geopolitik.Ontology.User
import Geopolitik.Ontology.Key
import Data.Time.Clock
import Data.Text
import GHC.Generics
import Data.Aeson
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow

data Session = Session
  { sessionID :: Key Session
  , sessionOwner :: Key User
  , sessionCreationDate :: UTCTime
  , sessionToken :: Text
  } deriving stock (Generic, Eq, Show, Read, Ord)
    deriving anyclass (FromRow, ToRow, ToJSON, FromJSON)
