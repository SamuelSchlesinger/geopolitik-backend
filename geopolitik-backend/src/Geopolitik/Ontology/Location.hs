module Geopolitik.Ontology.Location where

import Data.Text
import Database.PostgreSQL.Simple.FromField
import Geopolitik.Ontology.Key
import Geopolitik.Ontology.Article
import Data.Time.Clock
import GHC.Generics
import Database.PostgreSQL.Simple.FromRow
import Data.Aeson
import Data.Text.Read

data Location = Location
  { locationID :: Key Location
  , locationName :: Text
  , locationDescription :: Key Article
  , locationSpot :: Coordinate
  , locationCreationDate :: UTCTime
  } deriving stock (Generic, Eq, Show, Read, Ord)
    deriving anyclass (FromRow, ToJSON, FromJSON)

data Coordinate = Coordinate Double Double
  deriving stock (Generic, Ord, Eq, Show, Read)
  deriving anyclass (ToJSON, FromJSON)

instance FromField Coordinate where
  fromField f bs = (splitOn "(" <$> fromField f bs) >>= \case
      ["Point", point, ""] -> case splitOn " " point of
        [tx, ty] ->
          case (double tx, double ty) of
            (Right (x, ""), Right (y, "")) -> return $ Coordinate x y
            (_, _) -> conversionError (userError "reee")
        _ -> conversionError (userError "reeee")
      _ -> conversionError (userError "reeeee")
