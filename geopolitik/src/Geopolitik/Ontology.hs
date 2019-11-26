{-# LANGUAGE DeriveAnyClass #-}
module Geopolitik.Ontology where

import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.Time.Clock (UTCTime)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField

newtype Key a = Key { getKey :: Text }
  deriving newtype (Eq, Show, Read, Hashable, Ord, FromField, ToField)

data User = User
  { username :: Key User 
  , password :: ByteString 
  , userCreationDate :: UTCTime 
  } deriving stock (Generic, Eq, Show, Read, Ord)
    deriving anyclass (FromRow, ToRow)

data Article = Article
  { articleName :: Text
  , articleAuthor :: Key User
  , articleCreationDate :: UTCTime 
  } deriving stock (Generic, Eq, Show, Read, Ord) 
    deriving anyclass (FromRow, ToRow)

data Draft = Draft
  { draftArticle :: Key Article
  , draftContents :: Text
  , draftCreationDate :: UTCTime 
  , draftAuthor :: Key User 
  } deriving stock (Generic, Eq, Show, Read, Ord)
    deriving anyclass (FromRow, ToRow)

class Owned d where
  owner :: d -> Key User

instance Owned User where
  owner = username

instance Owned Article where
  owner = articleAuthor

instance Owned Draft where
  owner = draftAuthor

class Dated d where
  creationDate :: d -> UTCTime

instance Dated User where
  creationDate = userCreationDate

instance Dated Article where
  creationDate = articleCreationDate

instance Dated Draft where
  creationDate = draftCreationDate
