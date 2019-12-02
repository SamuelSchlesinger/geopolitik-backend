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
  { userID :: Key User
  , username :: Text
  , password :: Text
  , userCreationDate :: UTCTime 
  } deriving stock (Generic, Eq, Show, Read, Ord)
    deriving anyclass (FromRow, ToRow)

data Article = Article
  { articleID :: Key Article
  , articleName :: Text
  , articleOwner :: Key User
  , articleCreationDate :: UTCTime 
  } deriving stock (Generic, Eq, Show, Read, Ord) 
    deriving anyclass (FromRow, ToRow)

data Draft = Draft
  { draftID :: Key Draft
  , draftArticle :: Key Article
  , draftContents :: Text
  , draftAuthor :: Key User
  , draftCreationDate :: UTCTime 
  } deriving stock (Generic, Eq, Show, Read, Ord)
    deriving anyclass (FromRow, ToRow)

data Session = Session
  { sessionID :: Key Session
  , sessionOwner :: Key User 
  , sessionCreationDate :: UTCTime
  , sessionData :: ByteString
  } deriving stock (Generic, Eq, Show, Read, Ord)
    deriving anyclass (FromRow, ToRow)
