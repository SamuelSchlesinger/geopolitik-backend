module Geopolitik where

import Servant.Server
import Geopolitik.Server
import Geopolitik.Database
import Database.PostgreSQL.Simple
import Control.Monad.Reader.Class
import Network.Wai.Handler.Warp
import Control.Monad.Trans
import System.Posix.User
import System.Directory
import System.Environment
import Network.Wai.Middleware.RequestLogger

main :: IO ()
main = do
  getEnv "GEOPOLITIK_LOCATION" >>= setCurrentDirectory
  username <- getEffectiveUserName
  let connInfo = ConnectInfo { 
      connectHost = "localhost"
    , connectPort = 5432
    , connectUser = username
    , connectPassword = ""
    , connectDatabase = "geopolitik"
    }  
  runDatabaseT connInfo do
    conn <- ask
    let app = serveWithContext 
                geopolitikProxy 
                (ctx conn) 
                (hoistServerWithContext geopolitikProxy ctxProxy
                   (runSharedDatabaseT conn) 
                   server)
    let settings = setOnException errorPrinter $ setPort 8080 defaultSettings
    liftIO (runSettings settings $ logStdoutDev app)

errorPrinter :: (Show a, Show b) => a -> b -> IO ()
errorPrinter _ ex = do
  putStr "EXCEPTION:"
  print ex
