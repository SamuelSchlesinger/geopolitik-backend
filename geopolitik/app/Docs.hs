{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Geopolitik.API
import Geopolitik.Ontology
import Servant.Docs hiding (Response)
import Data.Proxy
import Data.Text (Text)
import Web.Cookie (SetCookie)
import Data.Time.Clock
import Servant
import Data.Void
import Control.Lens

instance ToSample SignUp
instance ToSample (Response SignUp)
instance ToSample SignIn
instance ToSample (Response SignIn)
instance ToSample NewArticle where
instance ToSample (Key Draft)
instance ToSample (Key Article)
instance ToSample (Key User)
instance ToSample (Key Void)
instance ToSample (Key Comment)
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

instance ToSample ()

instance ToSample Text where
  toSamples Proxy = [("text", "yes")]

instance ToSample SetCookie where
  toSamples Proxy = []

instance ToSample (Response AddCollaborator)

instance ToSample Char where
  toSamples Proxy = [("", x) | x <- ['a'..'z']]

instance ToSample AddCollaborator

instance HasDocs p => HasDocs (AuthProtect "geopolitik-user" :> p) where
  docsFor Proxy (e, a) = docsFor (Proxy @p) (e, a & authInfo %~ (<> [geoAuthDoc])) where
    geoAuthDoc :: DocAuthentication
    geoAuthDoc = DocAuthentication "A simple HTTP-only session based authentication system" "A session cookie"

main :: IO ()
main = putStrLn . markdown . docs $ Proxy @GeopolitikAPI
