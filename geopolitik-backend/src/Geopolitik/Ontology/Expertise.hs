module Geopolitik.Ontology.Expertise where

import Geopolitik.Ontology.Key
import Geopolitik.Ontology.Article
import Data.Text
import GHC.Generics
import Database.PostgreSQL.Simple
import Data.Time.Clock

data Expertise = Expertise
  { expertiseID :: Key Expertise
  , expertiseName :: Text
  , expertiseDescription :: Key Article
  , expertiseCreationDate :: UTCTime
  } deriving (Eq, Ord, Generic, ToRow, FromRow)
