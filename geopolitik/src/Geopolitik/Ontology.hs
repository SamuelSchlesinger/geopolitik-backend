{-# LANGUAGE DeriveAnyClass #-}
module Geopolitik.Ontology where

import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Data.Aeson (ToJSON, FromJSON)
import Servant

newtype Key a = Key { getKey :: Text }
  deriving newtype (FromHttpApiData, Eq, Show, Read, Ord, FromField, ToField, ToJSON, FromJSON)

data User = User
  { userID :: Key User
  , username :: Text
  , password :: Text
  , userCreationDate :: UTCTime 
  } deriving stock (Generic, Eq, Show, Read, Ord)
    deriving anyclass (FromRow, ToRow, ToJSON, FromJSON)

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
  
