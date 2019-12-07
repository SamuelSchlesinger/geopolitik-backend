{-# LANGUAGE DeriveAnyClass #-}
module Geopolitik.API where

import Servant
import Servant.Server.Experimental.Auth
import Data.Text (Text)
import Geopolitik.Ontology
import Data.Void
import Data.Time.Clock (UTCTime)
import qualified Network.Wai as Wai
import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)
import Web.Cookie

type GeopolitikAPI = "account" :> AccountAPI 
                :<|> "article" :> ArticleAPI
                :<|> Raw

type AccountAPI = "signup" :> P SignUp
             :<|> "signin" :> C SignIn
             :<|> AuthProtect "geopolitik-user"
                  :> ( "new-token" :> S (Response SignIn) )

type ArticleAPI = AuthProtect "geopolitik-user" :> 
                ( "new"    :> P NewArticle
             :<|> "draft"  :> DraftAPI ) 
             
type DraftAPI = "new"      :> P NewDraft
           :<|> "comments" :> G "draft-key" DraftComments
           :<|> "link"   :> P LinkDraft
           :<|> "latest" :> G "article-key" LatestDraft 
           :<|> Capture "username" Text :> Capture "article-name" Text :> Get '[JSON] (Response LatestDraft) 

type Ctx = AuthHandler Wai.Request User ': '[]

data family Response a

type family Request b where
  Request (Response a) = a

type instance AuthServerData (AuthProtect "geopolitik-user") = User

type P req = ReqBody '[JSON] req 
          :> Post '[JSON] (Response req)

type G t req = Capture t req 
            :> Get '[JSON] (Response req) 

type C req = ReqBody '[JSON] req 
          :> S (Response SignIn)

type S res = Post '[JSON] (H res) 

type H res = Headers '[ Header "Set-Cookie" SetCookie
                      , Header "Access-Control-Allow-Origin" String
                      , Header "Access-Control-Allow-Credentials" Bool ] res

data SignUp = SignUp 
  { signUpUsername :: Text
  , signUpPassword :: Text }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON)

data instance Response SignUp = SignedUp
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON)

data SignIn = SignIn
  { signInUsername :: Text
  , signInPassword :: Text }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON)

data instance Response SignIn = SignedIn Text
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype NewArticle = NewArticle
  { newArticleName :: Text }
  deriving newtype (Eq, Ord, Show, Read, FromJSON, ToJSON)

data instance Response NewArticle = ArticleCreated (Key Article)
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON)

data NewDraft = NewDraft
  { newDraftArticle :: Key Article
  , newDraftContents :: Text }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON)

data instance Response NewDraft = DraftCreated
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype LatestDraft = LatestDraft { unLatestDraft :: Key Article }
  deriving newtype (Eq, Ord, Show, Read, FromJSON, ToJSON, FromHttpApiData)

data instance Response LatestDraft = LatestDraftFound (Key Draft) (Key User) Text UTCTime | LatestDraftNotFound
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype DraftComments = DraftComments { unDraftComments :: Key Draft }
  deriving newtype (Eq, Ord, Show, Read, FromJSON, ToJSON, FromHttpApiData)

data instance Response DraftComments = DraftCommentsFound [Comment]
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON)

data LinkDraft = LinkDraft
  { linkDraftFrom :: Key Draft
  , linkDraftTag :: SomeTag
  , linkDraftTo :: Key Void 
  } deriving stock (Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

data instance Response LinkDraft = Linked
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON)
