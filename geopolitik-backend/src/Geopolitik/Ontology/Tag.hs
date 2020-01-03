module Geopolitik.Ontology.Tag where

import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Data.Aeson
import Geopolitik.Ontology.Key
import Geopolitik.Ontology.Article
import Geopolitik.Ontology.User
import Geopolitik.Ontology.Comment
import Geopolitik.Ontology.Location
import Geopolitik.Ontology.Draft
import Geopolitik.Ontology.Expertise
import Data.Void
import Data.Text

data Tag a where
  ArticleTag :: Tag Article
  UserTag :: Tag User
  CommentTag :: Tag Comment
  LocationTag :: Tag Location
  DraftTag :: Tag Draft
  ExpertiseTag :: Tag Expertise

deriving instance Eq (Tag a)
deriving instance Ord (Tag a)
deriving instance Show (Tag a)

data SomeTag = forall a. SomeTag (Tag a)

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
