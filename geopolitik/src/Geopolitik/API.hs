module Geopolitik.API where

import Servant
import Data.Text (Text)
import Geopolitik.Ontology
import Data.Time.Clock (UTCTime)

type GeopolitikAPI = "account" :> AccountAPI :<|> "article" :>  ArticleAPI

data family Response a

type family Request b where
  Request (Response a) = a

type P req = ReqBody '[JSON] req :> Post '[JSON] (Response req)

type G t req = Capture t req :> Get '[JSON] (Response req) 

type AccountAPI = "signup" :> P SignUp
             :<|> "signin" :> P SignIn

type ArticleAPI = "new"    :> P NewArticle
             :<|> "draft"  :> DraftAPI 
             
type DraftAPI = "new"      :> P NewDraft
           :<|> "latest"   :> G "article-key" LatestDraft
  
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
