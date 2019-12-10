module Geopolitik.Ontology.Key where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Servant

newtype Key a = Key { getKey :: Text }
  deriving stock (Generic)
  deriving newtype (ToHttpApiData, FromHttpApiData, Eq, Show, Read, Ord, FromField, ToField, ToJSON, FromJSON)
