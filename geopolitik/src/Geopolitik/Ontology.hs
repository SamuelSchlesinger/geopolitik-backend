{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
module Geopolitik.Ontology where

import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Data.Aeson
import Data.Text.Read
import Data.Text
import Data.Void
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
  , draftAuthor :: Key User
  , draftContents :: Text
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

data Tag a where
  ArticleTag :: Tag Article
  UserTag :: Tag User
  CommentTag :: Tag Comment
  LocationTag :: Tag Location
  DraftTag :: Tag Draft

deriving instance Eq (Tag a)
deriving instance Ord (Tag a)
deriving instance Show (Tag a)

data SomeTag = forall a. SomeTag (Tag a) 

data Link = Link 
  { linkID :: Key Link
  , linkTag :: SomeTag
  , linkDraft :: Key Draft
  , linkEntity :: Key Void }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

-- AlWAYS put all of the tags here, this todo lives forever
allTags :: [SomeTag]
allTags = [SomeTag ArticleTag, SomeTag UserTag, SomeTag CommentTag, SomeTag LocationTag, SomeTag DraftTag]

instance Eq SomeTag where
  SomeTag a == SomeTag b = show a == show b

instance ToField SomeTag where
  toField (SomeTag a) = toField (pack (show a) :: Text)

instance FromField SomeTag where
  fromField field bs = do
    tag <- fromField field bs
    case [ SomeTag tag' | SomeTag tag' <- allTags, tag == pack (show tag') ] of
      x : [] -> return x
      _ -> conversionError (userError "reee")

instance ToJSON SomeTag where
  toJSON (SomeTag tag) = toJSON (show tag)

instance FromJSON SomeTag where
  parseJSON a = case [ SomeTag tag' | SomeTag tag' <- allTags, fromJSON a == Success (toJSON (show tag')) ] of
    x : [] -> return x
    _ -> fail "reeeee"

obscure :: Key a -> Key Void
obscure (Key a) = Key a

obvious :: Tag a -> Key Void -> Key a
obvious _ (Key a) = Key a
