{-# LANGUAGE DeriveAnyClass #-}

module Geopolitik.API where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Void
import GHC.Generics (Generic)
import Geopolitik.Ontology
import Servant
import Servant.Server.Experimental.Auth
import Web.Cookie
import qualified Network.Wai as Wai

type GeopolitikAPI
     = "account" :> AccountAPI 
  :<|> "article" :> ArticleAPI 
  :<|> Raw

type AccountAPI
     = "signup" :> P SignUp 
  :<|> "signin" :> C SignIn 
  :<|> AuthProtect "geopolitik-user" :> ("new-token" :> S (Response SignIn))

type ArticleAPI
   = AuthProtect "geopolitik-user" :> UnauthenticatedArticleAPI 

type UnauthenticatedArticleAPI
    = "new" :> P NewArticle
 :<|> "draft" :> DraftAPI
 :<|> "collaborator" :> CollaboratorAPI 

type DraftAPI
     = "new" :> P NewDraft
  :<|> "comments" :> G "draft-key" DraftComments 
  :<|> "link" :> P LinkDraft 
  :<|> "latest" :> G "article-key" LatestDraft 
  :<|> Capture "username" Text :> Capture "article-name" Text :> Get '[JSON] (Response LatestDraft)

type CollaboratorAPI
    = "add" :> P AddCollaborator

type Ctx = AuthHandler Wai.Request User ': '[]

data family Response a

type family Request b where
  Request (Response a) = a

type instance AuthServerData (AuthProtect "geopolitik-user") = User

type P req = ReqBody '[JSON] req :> Post '[JSON] (Response req)

type G t req = Capture t req :> Get '[JSON] (Response req)

type C req = ReqBody '[JSON] req :> S (Response SignIn)

type S res = Post '[JSON] (H res)

type H res
   = Headers '[ Header "Set-Cookie" SetCookie
              , Header "Access-Control-Allow-Origin" String
              , Header "Access-Control-Allow-Headers" String
              , Header "Access-Control-Allow-Credentials" Bool ] res

data SignUp = SignUp
  { signUpUsername :: Text
  , signUpPassword :: Text }
  deriving  (Eq, Ord, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON)

data instance Response SignUp = SignedUp
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON)

data SignIn = SignIn
  { signInUsername :: Text
  , signInPassword :: Text }
  deriving  (Eq, Ord, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON)

data instance Response SignIn = SignedIn Text
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype NewArticle = NewArticle
  { newArticleName :: Text }
  deriving stock (Generic)
  deriving newtype (Eq, Ord, Show, Read, FromJSON, ToJSON)

data instance Response NewArticle 
  = ArticleCreated (Key Article)
    deriving stock (Eq, Ord, Show, Read, Generic)
    deriving anyclass (FromJSON, ToJSON)

data NewDraft = NewDraft
  { newDraftArticle :: Key Article
  , newDraftContents :: Text }
  deriving  (Eq, Ord, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON)

data instance Response NewDraft = DraftCreated
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype LatestDraft = LatestDraft
  { unLatestDraft :: Key Article }
  deriving newtype (Eq, Ord, Show, Read, FromJSON, ToJSON, FromHttpApiData, ToHttpApiData)

data instance  Response LatestDraft 
  = LatestDraftFound (Key Draft) (Key User) Text UTCTime
    deriving stock (Eq, Ord, Show, Read, Generic)
    deriving anyclass (FromJSON, ToJSON)

newtype DraftComments = DraftComments
  { unDraftComments :: Key Draft }
  deriving newtype (Eq, Ord, Show, Read, FromJSON, ToJSON, FromHttpApiData, ToHttpApiData)

data instance Response DraftComments 
  = DraftCommentsFound [Comment]
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON)

data LinkDraft = LinkDraft
  { linkDraftFrom :: Key Draft
  , linkDraftTag :: SomeTag
  , linkDraftTo :: Key Void }
  deriving  (Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data instance Response LinkDraft = Linked
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON)

data AddCollaborator = AddCollaborator
  { addCollaboratorUsername :: Text
  , addCollaboratorArticle :: Key Article
  } deriving (Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

data instance Response AddCollaborator = AddedCollaborator
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON)
