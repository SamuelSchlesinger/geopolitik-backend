{-# LANGUAGE DeriveAnyClass #-}
module Geopolitik.API where

import Servant
import Servant.Server.Experimental.Auth
import Data.Text (Text)
import Geopolitik.Ontology
import Data.Time.Clock (UTCTime)
import qualified Network.Wai as Wai
import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)
import Web.Cookie

type Ctx = AuthHandler Wai.Request User ': '[]

type GeopolitikAPI = "account" :> AccountAPI :<|> "article" :> ArticleAPI

data family Response a

type family Request b where
  Request (Response a) = a

type instance AuthServerData (AuthProtect "geopolitik-user") = User

type P req = ReqBody '[JSON] req 
          :> Post '[JSON] (Response req)

type G t req = Capture t req 
            :> Get '[JSON] (Response req) 

type C req = ReqBody '[JSON] req 
          :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie] (Response req))

type AccountAPI = "signup" :> P SignUp
             :<|> "signin" :> C SignIn
             :<|> AuthProtect "geopolitik-user"
                  :> ( "new-token" :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie] ()))

type ArticleAPI = AuthProtect "geopolitik-user" :> 
                ( "new"    :> P NewArticle
             :<|> "link"   :> P LinkArticle
             :<|> "draft"  :> DraftAPI ) 
             
type DraftAPI = "new"      :> P NewDraft
           :<|> "latest"   :> G "article-key" LatestDraft
           :<|> Capture "username" Text :> Capture "article-name" Text :> Get '[JSON] (Response LatestDraft) 

data SignUp = SignUp 
  { signUpUsername :: Text
  , signUpPassword :: Text }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON)

data instance Response SignUp = SignedUp | SignUpFailure
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON)

data SignIn = SignIn
  { signInUsername :: Text
  , signInPassword :: Text }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON)

data instance Response SignIn = SignedIn | SignInFailure
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype NewArticle = NewArticle
  { newArticleName :: Text }
  deriving newtype (Eq, Ord, Show, Read, FromJSON, ToJSON)

data instance Response NewArticle = ArticleCreated | ArticleCreationFailure
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON)

data NewDraft = NewDraft
  { newDraftArticle :: Key Article
  , newDraftContents :: Text }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON)

data instance Response NewDraft = DraftCreated | DraftCreationFailure
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype LatestDraft = LatestDraft { unLatestDraft :: Key Article }
  deriving newtype (Eq, Ord, Show, Read, FromJSON, ToJSON, FromHttpApiData)

data instance Response LatestDraft = LatestDraftFound (Key Draft) (Key User) Text UTCTime | LatestDraftNotFound
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON)

data LinkArticle = LinkArticle
  { linkArticleFrom :: Key Article
  , linkArticleTo :: Key Article 
  } deriving stock (Eq, Ord, Show, Read, Generic)
    deriving anyclass (FromJSON, ToJSON)

data instance Response LinkArticle = LinkedArticles | LinkArticleFailure
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON)
