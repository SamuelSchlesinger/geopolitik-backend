module Geopolitik.API where

import Servant
import Servant.Server.Experimental.Auth
import Data.Text (Text)
import Geopolitik.Ontology
import Data.Time.Clock (UTCTime)
import qualified Network.Wai as Wai

type Ctx = AuthHandler Wai.Request User ': '[]

type GeopolitikAPI = "account" :> AccountAPI :<|> "article" :> ArticleAPI

data family Response a

type family Request b where
  Request (Response a) = a

type instance AuthServerData (AuthProtect "user") = User

type P req = ReqBody '[JSON] req :> Post '[JSON] (Response req)

type G t req = Capture t req :> Get '[JSON] (Response req) 

type AccountAPI = "signup" :> P SignUp
             :<|> "signin" :> P SignIn

type ArticleAPI = AuthProtect "user" :> 
                ( "new"    :> P NewArticle
             :<|> "draft"  :> DraftAPI ) 
             
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
  { newArticleName :: Text }

data instance Response NewArticle = ArticleCreated | ArticleCreationFailure

data NewDraft = NewDraft
  { newDraftArticle :: Key Article
  , newDraftContents :: Text }

data instance Response NewDraft = DraftCreated | DraftCreationFailure

newtype LatestDraft = LatestDraft { unLatestDraft :: Key Article }

data instance Response LatestDraft = LatestDraftFound (Key Draft) (Key User) Text UTCTime | LatestDraftNotFound
