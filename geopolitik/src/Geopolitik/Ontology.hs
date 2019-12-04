{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
module Geopolitik.Ontology where

import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.Text.Read
import Data.Text
import Servant hiding (Link)

newtype Key a = Key { getKey :: Text }
  deriving newtype (FromHttpApiData, Eq, Show, Read, Ord, FromField, ToField, ToJSON, FromJSON)

data User = User
  { userID :: Key User
  , username :: Text
  , password :: Text
  , userCreationDate :: UTCTime 
  } deriving stock (Generic, Eq, Show, Read, Ord)
    deriving anyclass (FromRow, ToJSON, FromJSON)

data Article = Article
  { articleID :: Key Article
  , articleName :: Text
  , articleOwner :: Key User
  , articleCreationDate :: UTCTime 
  } deriving stock (Generic, Eq, Show, Read, Ord) 
    deriving anyclass (FromRow, ToRow, ToJSON, FromJSON)

data Draft = Draft
  { draftID :: Key Draft
  , draftArticle :: Key Article
  , draftContents :: Text
  , draftAuthor :: Key User
  , draftCreationDate :: UTCTime 
  } deriving stock (Generic, Eq, Show, Read, Ord)
    deriving anyclass (FromRow, ToRow, ToJSON, FromJSON)

data Session = Session
  { sessionID :: Key Session
  , sessionOwner :: Key User 
  , sessionCreationDate :: UTCTime
  , sessionToken :: Text
  } deriving stock (Generic, Eq, Show, Read, Ord)
    deriving anyclass (FromRow, ToRow, ToJSON, FromJSON)

data ExecutedMigration = ExecutedMigration
  { executedMigrationFilePath :: FilePath 
  , executedMigrationTimestamp :: UTCTime
  } deriving stock (Generic, Eq, Show, Read, Ord)
    deriving anyclass (FromRow, ToRow, ToJSON, FromJSON) 

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
  fromField field bs = (splitOn "(" <$> fromField field bs) >>= \case
      ["Point", point, ""] -> case splitOn " " point of
        [tx, ty] ->
          case (double tx, double ty) of
            (Right (x, ""), Right (y, "")) -> return $ Coordinate x y
            (_, _) -> conversionError (userError "reee")
        _ -> conversionError (userError "reeee")
      _ -> conversionError (userError "reeeee")


data Comment = Comment
  { commentID :: Key Comment
  , commentAuthor :: Key User
  , commentContent :: Text 
  } deriving stock (Generic, Eq, Show, Read, Ord)
    deriving anyclass (ToRow, FromRow, ToJSON, FromJSON) 
