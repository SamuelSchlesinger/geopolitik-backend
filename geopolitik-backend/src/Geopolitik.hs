module Geopolitik where

import Control.Monad.Reader.Class
import Control.Monad.Trans
import Data.Time.Clock
import Geopolitik.Database
import Geopolitik.Server
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger
import Servant.Server
import System.Directory
import System.Environment
import System.IO
import System.Posix.User

main :: IO ()
main = do
  getEnv "GEOPOLITIK_LOCATION" >>= setCurrentDirectory
  connInfo <- testInfo <$> getEffectiveUserName
  runDatabaseT connInfo do
    conn <- ask
    let app = serveWithContext 
                geopolitikProxy 
                (ctx conn) 
                (hoistServerWithContext geopolitikProxy ctxProxy
                   (runSharedDatabaseT conn) 
                   server)
    let settings = setOnException exceptionPrinter $ setPort 8080 defaultSettings
    liftIO (runSettings settings $ logStdoutDev app)

exceptionPrinter :: (Show a, Show b) => a -> b -> IO ()
exceptionPrinter _ ex = do
  now <- getCurrentTime
  hPutStr stderr $ "Exception @ " <> show now <> ": "
  print ex
