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
    liftIO (runSettings settings app)

errorPrinter :: (Show a, Show b) => a -> b -> IO ()
errorPrinter sr ex = do
  putStr "This request: "
  print sr
  putStr "Fails with this exception: "
  print ex
