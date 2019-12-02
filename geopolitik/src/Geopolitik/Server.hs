module Geopolitik.Server where

import Geopolitik.Database
import Geopolitik.API
import Servant

type M = DatabaseT Handler

server :: ServerT GeopolitikAPI M
server = account :<|> article

account :: ServerT AccountAPI M
account = signup :<|> signin

signup :: SignUp -> M (Response SignUp)
signup SignUp{..} = undefined

signin :: SignIn -> M (Response SignIn)
signin SignIn{..} = undefined

article :: ServerT ArticleAPI M
article = newArticle :<|> draft

newArticle :: NewArticle -> DatabaseT Handler (Response NewArticle)
newArticle NewArticle{..} = undefined

draft :: ServerT DraftAPI M
draft = newDraft :<|> latest

newDraft :: NewDraft -> M (Response NewDraft)
newDraft NewDraft{..} = undefined

latest :: LatestDraft -> M (Response LatestDraft)
latest LatestDraft{..} = undefined
