module Geopolitik.Ontology.Expert where

import Geopolitik.Ontology.Key
import Geopolitik.Ontology.User
import GHC.Generics
import Database.PostgreSQL.Simple
import Data.Time.Clock
import Geopolitik.Ontology.Expertise

data Expert = Expert
  { expertID :: Key User
  , expertExpertise :: Key Expertise
  , expertCreationDate :: UTCTime
  } deriving (Eq, Ord, Generic, ToRow, FromRow)
