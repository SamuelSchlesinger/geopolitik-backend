module Geopolitik where

import Servant.Server
import Geopolitik.Server
import Geopolitik.Database
import Database.PostgreSQL.Simple
import Control.Monad.Reader.Class
import Network.Wai.Handler.Warp
import Control.Monad.Trans

main :: IO ()
main = do
  let connInfo = ConnectInfo { 
      connectHost = "localhost"
    , connectPort = 5432
    , connectUser = "sam"
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
    liftIO (run 8080 app)
