module Geopolitik.API where

import Servant
import Data.Text (Text)
import Geopolitik.Ontology
import Data.Time.Clock (UTCTime)

type GeopolitikAPI = "account" :> AccountAPI :<|> "article" :>  ArticleAPI

data family Response a

type family Request b where
  Request (Response a) = a

type SimplePost req res = ReqBody '[JSON] req :> Post '[JSON] res

type AccountAPI = "signup" :> SimplePost SignUp (Response SignUp)
             :<|> "signin" :> SimplePost SignIn (Response SignIn)

type ArticleAPI = "new"    :> SimplePost NewArticle (Response NewArticle)
             :<|> "draft"  :> SimplePost NewDraft (Response NewDraft)
             :<|> "latest" :> Capture "article-key" LatestDraft :> Get '[JSON] (Response LatestDraft)
  
data SignUp = SignUp 
  { signUpUsername :: Text
  , signUpPassword :: Text }

data instance Response SignUp = SignedUp | SignUpFailure

data SignIn = SignIn
  { signInUsername :: Text
  , signInPassword :: Text }

data instance Response SignIn = SignedIn | SignInFailure

data NewArticle = NewArticle
  { newArticleName :: Text
  , newArticleAuthor :: Key User }

data instance Response NewArticle = ArticleCreated | ArticleCreationFailure

data NewDraft = NewDraft
  { newDraftArticle :: Key Article
  , newDraftContents :: Text }

data instance Response NewDraft = DraftCreated | DraftCreationFailure

newtype LatestDraft = LatestDraft { unLatestDraft :: Key Article }

data instance Response LatestDraft = LatestDraftFound (Key Draft) (Key User) Text UTCTime | LatestDraftNotFound
