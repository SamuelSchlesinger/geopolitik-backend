{-# LANGUAGE DeriveAnyClass #-}
module Geopolitik.Tag where

import Database.PostgreSQL.Simple.ToField
import GHC.Generics
import Database.PostgreSQL.Simple.FromField
import Data.Aeson (FromJSON(..), ToJSON(..), fromJSON, Result(Success))
import Geopolitik.Ontology
import Data.Text (Text, pack)
import Data.Void

data Tag a where
  ArticleTag :: Tag Article
  UserTag :: Tag User
  CommentTag :: Tag Comment
  LocationTag :: Tag Location

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
allTags = [SomeTag ArticleTag, SomeTag UserTag, SomeTag CommentTag, SomeTag LocationTag]

instance Show SomeTag where
  show (SomeTag UserTag) = "UserTag"
  show (SomeTag ArticleTag) = "ArticleTag"
  show (SomeTag CommentTag) = "CommentTag"
  show (SomeTag LocationTag) = "LocationTag"

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
