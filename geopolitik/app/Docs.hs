module Main where

import Geopolitik.API
import Geopolitik.Ontology
import Geopolitik.Tag
import Servant.Docs hiding (Response)
import Data.Proxy
import Data.Text (Text)
import Web.Cookie (SetCookie)
import Data.Time.Clock
import Servant
import Data.Void

instance ToSample SignUp
instance ToSample (Response SignUp)
instance ToSample SignIn
instance ToSample (Response SignIn)
instance ToSample NewArticle where
  toSamples Proxy = [("an article name", NewArticle "my brand new article")]

instance ToSample (Key Draft) where
  toSamples Proxy = [("draft key", Key "b00e6dec-fcb4-4e4a-a298-c5cac17e37e8")]

instance ToSample (Key Article) where
  toSamples Proxy = [("article key", Key "b00e6dec-fcb4-4e4a-a298-c5cac17e37e8")]

instance ToSample (Key User) where
  toSamples Proxy = [("user key", Key "b00e6dec-fcb4-4e4a-a298-c5cac17e37e8")]

instance ToSample (Key Void) where
  toSamples Proxy = []

instance ToSample (Key Comment) where
  toSamples Proxy = [("comment key", Key "b00e6dec-fcb4-4e4a-a298-c5cac17e37e8")]

instance ToSample (Response NewArticle)
instance ToSample (Response LinkDraft)
instance ToSample LinkDraft
instance ToSample (Response NewDraft)
instance ToSample NewDraft
instance ToSample (Response LatestDraft)
instance ToSample Comment
instance ToSample Draft

instance ToSample UTCTime where
  toSamples _ = []

instance ToCapture (Capture "article-key" LatestDraft) where
  toCapture _ = DocCapture "article-key" "Article key"

instance ToCapture (Capture "username" Text) where
  toCapture _ = DocCapture "username" "Username"

instance ToCapture (Capture "article-name" Text) where
  toCapture _ = DocCapture "article-name" "Article name"

instance ToCapture (Capture "draft-key" DraftComments) where
  toCapture _ = DocCapture "draft-key" "Draft key"

instance ToSample (Response DraftComments)

instance ToSample SomeTag where
  toSamples Proxy = [("an article tag", SomeTag ArticleTag)]

instance ToSample () where
  toSamples Proxy = []

instance ToSample Text where
  toSamples Proxy = [("some text", "as such")]

instance ToSample SetCookie where
  toSamples Proxy = []

instance HasDocs p => HasDocs (AuthProtect "geopolitik-user" :> p) where
  docsFor Proxy = docsFor (Proxy @p)

main :: IO ()
main = putStrLn . markdown . docs $ Proxy @GeopolitikAPI
