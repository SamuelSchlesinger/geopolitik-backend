module Geopolitik.Ontology.User where

import Data.Aeson
import Geopolitik.Ontology.Key
import Data.Text
import Data.Time.Clock
import GHC.Generics (Generic)
import Database.PostgreSQL.Simple.FromRow

data User = User
  { userID :: Key User
  , username :: Text
  , password :: Text
  , userCreationDate :: UTCTime
  } deriving stock (Generic, Eq, Show, Read, Ord)
    deriving anyclass (FromRow, ToJSON, FromJSON)
