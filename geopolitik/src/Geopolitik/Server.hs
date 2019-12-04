module Geopolitik.Server where

import Data.Text (Text)
import Geopolitik.Database
import Geopolitik.Ontology
import Control.Monad.Trans
import Geopolitik.API
import Servant
import Servant.Server.Experimental.Auth
import Data.Time.Clock
import Data.Text.Encoding
import Data.UUID
import System.Random
import qualified Network.Wai as Wai
import Database.PostgreSQL.Simple hiding ((:.))
import Database.PostgreSQL.Simple.SqlQQ
import Control.Monad.Reader.Class
import Web.Cookie
import Geopolitik.Tag

type M = DatabaseT Handler

geopolitikProxy :: Proxy GeopolitikAPI
geopolitikProxy = Proxy

ctxProxy :: Proxy Ctx
ctxProxy = Proxy

ctx :: Connection -> Context Ctx
ctx conn = mkAuthHandler validate :. EmptyContext where
  validate :: Wai.Request -> Handler User
  validate req = do
    cookie <- maybe (throwError err403) return 
      $ lookup "cookie" (Wai.requestHeaders req)
    token <- maybe (throwError err403) return
      $ lookup "geopolitik-user" $ parseCookies cookie
    maybe (throwError err401) return =<< runSharedDatabaseT conn (validateToken token) 

server :: ServerT GeopolitikAPI M
server = account :<|> article 

account :: ServerT AccountAPI M
account = signup :<|> signin :<|> newToken

signup :: SignUp -> M (Response SignUp)
signup (SignUp username password) 
  = lookupUsersByUsername [username] >>= \case
      [] -> do
        userCreationDate <- liftIO getCurrentTime
        userID <- liftIO ((Key . toText) <$> randomIO)
        insertUsers [User{..}] 
        return SignedUp
      _ -> return SignUpFailure

signin :: SignIn -> M (Headers '[Header "Set-Cookie" SetCookie] (Response SignIn))
signin (SignIn username password)
  = lookupUsersByUsername [username] >>= \case
      [User{userID = sessionOwner, password = userPassword}] -> do
        if (password == userPassword) then do
          sessionCreationDate <- liftIO getCurrentTime
          sessionToken <- liftIO (toText <$> randomIO)      
          sessionID <- liftIO ((Key . toText) <$> randomIO) 
          let sess = Session{..}
          insertSessions [sess]
          return $ addHeader (def 
                   { setCookieName = "geopolitik-user"
                   , setCookieMaxAge = Just (60 * 30)
                   , setCookieValue = encodeUtf8 sessionToken }) 
                   SignedIn 
        else throwError err403
      _ -> throwError err403

newToken :: User -> M (Headers '[Header "Set-Cookie" SetCookie] ())
newToken User{..} = do
  fmap (fmap (const ())) $ signin $ SignIn username password

article :: ServerT ArticleAPI M
article user = newArticle user :<|> draft user

newArticle :: User -> NewArticle -> DatabaseT Handler (Response NewArticle)
newArticle User{userID = articleOwner} (NewArticle articleName) = do
  articleCreationDate <- liftIO getCurrentTime
  articleID <- liftIO ((Key . toText) <$> randomIO) 
  insertArticles [Article{..}]
  return (ArticleCreated articleID)

link :: User -> LinkDraft -> DatabaseT Handler (Response LinkDraft)
link User{..} (LinkDraft linkDraft (SomeTag tag) linkEntity') = do
  case tag of
    ArticleTag -> do
      lookupDrafts [linkDraft] >>= \case
        [Draft{draftAuthor}] -> if userID == draftAuthor then do
          lookupArticles [obvious tag linkEntity'] >>= \case
            [Article{}] -> do 
              linkID <- liftIO ((Key . toText) <$> randomIO)
              let linkTag = SomeTag ArticleTag
              let linkEntity = obscure linkEntity'
              insertLinks [Link{..}] 
              return Linked
            [] -> throwError err404
            _ -> error "database is in an inconsistent state: link"
        else throwError err403
        [] -> throwError err404
        _ -> error "database is in an inconsistent state: link''''''"
    UserTag -> do
      lookupDrafts [linkDraft] >>= \case
        [Draft{draftAuthor}] -> if userID == draftAuthor then
          lookupUsers [obvious tag linkEntity'] >>= \case
            [User{}] -> do
              linkID <- liftIO ((Key . toText) <$> randomIO)
              let linkTag = SomeTag UserTag
              let linkEntity = obscure linkEntity
              insertLinks[Link{..}]
              return Linked
            [] -> throwError err404
            _ -> error "database is in an inconsistent state: link'" 
        else throwError err403
        [] -> throwError err404
        _ -> error "database is in an inconsistent state: link'''''"
    CommentTag -> do
      lookupComments [obvious tag linkEntity'] >>= \case
        [Comment{}] -> do
          linkID <- liftIO ((Key . toText) <$> randomIO)
          let linkTag = SomeTag CommentTag
          let linkEntity = obscure linkEntity'
          insertLinks [Link{..}]
          return Linked 
        [] -> throwError err404
        _ -> error "database is in an inconsistent state: link''"
    LocationTag -> do
      lookupDrafts [linkDraft] >>= \case
        [Draft{draftAuthor}] -> if userID == draftAuthor then
          lookupLocations [obvious tag linkEntity'] >>= \case
            [Location{}] -> do
              linkID <- liftIO ((Key . toText) <$> randomIO)
              let linkTag = SomeTag CommentTag
              let linkEntity = obscure linkEntity'
              insertLinks [Link{..}]
              return Linked
            [] -> throwError err404
            _ -> error "database is in an inconsistent state: link'''"
        else throwError err403
        [] -> throwError err404
        _ -> error "database is in an inconsistent state: link''''"

draft :: User -> ServerT DraftAPI M
draft user = newDraft user :<|> comments user :<|> link user :<|> latest user :<|> latest' user

comments :: User -> DraftComments -> M (Response DraftComments)
comments _ (DraftComments draftKey) = do
  c <- ask
  fmap DraftCommentsFound $ liftIO $ query c [sql|
     select (comments.id, comments.author, comments.content) from comments
        inner join links on comments.id = links.entity 
        where comments.id in ?
        and links.tag = 'CommentTag';
    |] (Only (In [draftKey]))

newDraft :: User -> NewDraft -> M (Response NewDraft)
newDraft u@User{userID = draftAuthor} (NewDraft draftArticle draftContents) = do
  draftCreationDate <- liftIO getCurrentTime
  draftID <- liftIO ((Key . toText) <$> randomIO)
  insertDraft u Draft{..}
  return DraftCreated

latest :: User -> LatestDraft -> M (Response LatestDraft)
latest _ (LatestDraft articleKey) 
  = latestDraft articleKey >>= \case
      Just Draft{..} -> return $ LatestDraftFound draftID draftAuthor draftContents draftCreationDate
      Nothing -> return LatestDraftNotFound

latest' :: User -> Text -> Text -> M (Response LatestDraft)
latest' _ username articleName
  = latestDraft' username articleName >>= \case
      Just Draft{..} -> return $ LatestDraftFound draftID draftAuthor draftContents draftCreationDate
      Nothing -> return LatestDraftNotFound
