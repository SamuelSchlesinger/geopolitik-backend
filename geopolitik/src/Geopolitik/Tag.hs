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

deriving instance Eq (Tag a)
deriving instance Ord (Tag a)
deriving instance Show (Tag a)

data SomeTag = forall a. SomeTag (Tag a) 

-- TODO put all of the tags here, this todo lives forever
tags :: [SomeTag]
tags = [SomeTag ArticleTag]

instance Show SomeTag where
  show (SomeTag ArticleTag) = "Article"

instance Eq SomeTag where
  SomeTag a == SomeTag b = show a == show b

instance ToField SomeTag where
  toField (SomeTag a) = toField (pack (show a) :: Text)

instance FromField SomeTag where
  fromField field bs = do
    tag <- fromField field bs
    case [ SomeTag tag' | SomeTag tag' <- tags, tag == pack (show tag') ] of
      x : [] -> return x
      _ -> conversionError (userError "reee")

instance ToJSON SomeTag where
  toJSON (SomeTag tag) = toJSON (show tag)

instance FromJSON SomeTag where
  parseJSON a = case [ SomeTag tag' | SomeTag tag' <- tags, fromJSON a == Success (toJSON (show tag')) ] of
    x : [] -> return x
    _ -> fail "reeeee"

data Link a = Link 
  { linkID :: Key (Link a)
  , linkTag :: SomeTag
  , linkArticle :: Key Article 
  , linkEntity :: Key Void }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

absurd :: Key a -> Key Void
absurd (Key a) = Key a

obvious :: Tag a -> Key Void -> Key a
obvious _ (Key a) = Key a
