{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Control.Lens
import Data.Proxy
import Data.Text (Text)
import Data.Time.Clock
import Data.Void
import Geopolitik.API
import Geopolitik.Ontology
import Servant
import Servant.Docs hiding (Response)
import Web.Cookie (SetCookie)

instance ToCapture (Capture "article-key" LatestDraft) where toCapture _ = DocCapture "article-key" "Article key"
instance ToCapture (Capture "article-name" Text) where toCapture _ = DocCapture "article-name" "Article name"
instance ToCapture (Capture "draft-key" DraftComments) where toCapture _ = DocCapture "draft-key" "Draft key"
instance ToCapture (Capture "username" Text) where toCapture _ = DocCapture "username" "Username"
instance ToSample ()
instance ToSample (Key Article)
instance ToSample (Key Comment)
instance ToSample (Key Draft)
instance ToSample (Key User)
instance ToSample (Key Void)
instance ToSample (Response AddCollaborator)
instance ToSample (Response DraftComments)
instance ToSample (Response LatestDraft)
instance ToSample (Response LinkDraft)
instance ToSample (Response NewArticle)
instance ToSample (Response NewDraft)
instance ToSample (Response SignIn)
instance ToSample (Response SignUp)
instance ToSample AddCollaborator
instance ToSample Char where toSamples Proxy = [("", x) | x <- ['a'..'z']]
instance ToSample Comment
instance ToSample Draft
instance ToSample LinkDraft
instance ToSample NewArticle
instance ToSample NewDraft
instance ToSample SetCookie where toSamples Proxy = []
instance ToSample SignIn
instance ToSample SignUp
instance ToSample SomeTag where toSamples Proxy = [("an article tag", SomeTag ArticleTag)]
instance ToSample Text where toSamples Proxy = [("text", "yes")]
instance ToSample UTCTime where toSamples _ = []

instance HasDocs p => HasDocs (AuthProtect "geopolitik-user" :> p) where
  docsFor Proxy (e, a) = docsFor (Proxy @p) (e, a & authInfo %~ (<> [geoAuthDoc])) where
    geoAuthDoc :: DocAuthentication
    geoAuthDoc = DocAuthentication "A simple HTTP-only session based authentication system" "A session cookie"

main :: IO ()
main = putStrLn . markdown . docs $ Proxy @GeopolitikAPI
