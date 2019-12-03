{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
module Geopolitik.Ontology where

import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Data.Aeson (ToJSON, FromJSON)
import Servant hiding (Link)

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

class ( Show (Tag a)
      , Read (Tag a)
      , Ord (Tag a)
      , Generic (Tag a)
      , FromRow a
      , ToRow a
      , ToField (Tag a)
      , FromField (Tag a)
      , ToJSON (Tag a)
      , FromJSON (Tag a)) => ArticleTag a where
  data Tag a
  tagTable :: Query

data Link a = Link 
  { linkID :: Key (Link a)
  , linkTag :: Tag a
  , linkArticle :: Key Article 
  , linkEntity :: Key a }

deriving instance ArticleTag a => Generic (Link a)
deriving instance ArticleTag a => Eq (Link a)
deriving instance ArticleTag a => Show (Link a)
deriving instance ArticleTag a => Read (Link a)
deriving instance ArticleTag a => Ord (Link a)
deriving instance ArticleTag a => FromRow (Link a)
deriving instance ArticleTag a => ToRow (Link a)
deriving instance ArticleTag a => ToJSON (Link a)
deriving instance ArticleTag a => FromJSON (Link a)
