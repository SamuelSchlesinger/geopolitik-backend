module Geopolitik where

import Servant.Server
import Geopolitik.Server
import Geopolitik.Database
import Database.PostgreSQL.Simple

main :: IO ()
main = do
  connection <- connect ConnectInfo { 
      connectHost = "localhost"
    , connectPort = 5432dip
    , connectUser = "sam"
    , connectPassword = ""
    , connectDatabase = "geopolitik"
    }  

  serveWithContext geopolitikProxy (ctx connection) (hoistServerWithContext geopolitikProxy ctxProxy _ server)
