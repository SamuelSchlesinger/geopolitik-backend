module Geopolitik.Server (geopolitikProxy, ctx, ctxProxy, server) where

import Control.Monad.Except
import Database.PostgreSQL.Simple hiding ((:.))
import Geopolitik.API
import Geopolitik.Database
import Geopolitik.Monad
import Geopolitik.Ontology
import Geopolitik.Server.Account
import Geopolitik.Server.Article
import Servant
import Servant.Server.Experimental.Auth
import Web.Cookie
import qualified Network.Wai as Wai

geopolitikProxy :: Proxy GeopolitikAPI
geopolitikProxy = Proxy

ctxProxy :: Proxy Ctx
ctxProxy = Proxy

ctx :: Connection -> Context Ctx
ctx conn = mkAuthHandler validate :. EmptyContext
  where
    validate :: Wai.Request -> Handler User
    validate req = do
      cookie <- maybe
          (throwError err401 {errReasonPhrase = "You have no cookies"})
          return $ lookup "cookie" (Wai.requestHeaders req)
      token <- maybe
          (throwError err401 {errReasonPhrase = "You don't have a geopolitik-user cookie"})
          return $ lookup "geopolitik-user" $ parseCookiesText cookie
      maybe
        (throwError err401 {errReasonPhrase = "Could not validate token"})
        return =<< runSharedDatabaseT conn (validateToken token)

server :: MonadGeopolitik m => ServerT GeopolitikAPI m
server = account :<|> article :<|> serveDirectoryWebApp "static"
