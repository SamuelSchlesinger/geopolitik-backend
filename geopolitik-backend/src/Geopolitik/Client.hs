module Geopolitik.Client
  ( ArticleClient(..)
  , CollaboratorClient(..)
  , DraftClient(..)
  , articleClient
  , newToken
  , signin
  , signup
  ) where

import Data.Proxy
import Data.Text
import Geopolitik.API
import Servant
import Servant.Client hiding (Response)
import Servant.Client.Core.Auth

signin :: SignIn -> ClientM (H (Response SignIn))

signup :: SignUp -> ClientM (Response SignUp)

newToken :: AuthenticatedRequest (AuthProtect "geopolitik-user") -> ClientM (H (Response SignIn))

rawArticleClient :: AuthenticatedRequest (AuthProtect "geopolitik-user") -> Client ClientM UnauthenticatedArticleAPI

articleClient :: AuthenticatedRequest (AuthProtect "geopolitik-user") -> ArticleClient
articleClient areq = ArticleClient{..} where
  newArticle :<|> (newDraft :<|> comments :<|> link :<|> latest :<|> latest') 
             :<|> (addCollaborator) = rawArticleClient areq
  draftClient = DraftClient{..}
  collaboratorClient = CollaboratorClient{..}

data ArticleClient = ArticleClient
  { newArticle :: NewArticle -> ClientM (Response NewArticle)
  , draftClient :: DraftClient 
  , collaboratorClient :: CollaboratorClient } 

data DraftClient = DraftClient
  { newDraft :: NewDraft -> ClientM (Response NewDraft)
  , comments :: DraftComments -> ClientM (Response DraftComments)
  , link :: LinkDraft -> ClientM (Response LinkDraft)
  , latest :: LatestDraft -> ClientM (Response LatestDraft)
  , latest' :: Text -> Text -> ClientM (Response LatestDraft) }

data CollaboratorClient = CollaboratorClient
  { addCollaborator :: AddCollaborator -> ClientM (Response AddCollaborator) }

(signup :<|> signin :<|> newToken) :<|> rawArticleClient :<|> _ = client (Proxy @ GeopolitikAPI)
