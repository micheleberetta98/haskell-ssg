{-|
  Module      : Server
  Description : Local server

  The local server defined here runs on a specified port (environment variable @PORT@)
  and serves a directory of HTML (and eventually static assets) files.
-}
module Server (Server, serve, kill, reload) where

import           Control.Concurrent
import           Data.Default.Class
import           Network.Wai.Handler.Warp
import           System.Environment
import           System.FilePath
import           Web.Scotty

-- | A type that wraps 'ThreadId' and represents a running server instance,
-- along its current directori
data Server = Server FilePath ThreadId

-- | Starts the server that redirects all requests to @dir@.
-- The port is read from the env variable PORT (default 4000).
serve :: FilePath -> IO Server
serve dir = do
  port <- getPortOr 4000
  putStrLn $ "Server running on port " <> show port <> "..."
  Server dir <$> forkIO (server dir port)

kill :: Server -> IO ()
kill (Server _ tid) = killThread tid

reload :: Server -> IO Server
reload (Server dir tid) = killThread tid >> serve dir

-- | Reads the env variable PORT
getPortOr :: Int -> IO Int
getPortOr defaultPort = maybe defaultPort read <$> lookupEnv "PORT"

-- | A simple server that maps requests to a specific file in a directory
server :: FilePath -> Int -> IO ()
server dir port = scottyOpts def{ verbose=0, settings=setPort port defaultSettings } $ do
  get (regex "^/(.*)$") $
    param "0" >>= rerouteTo dir

-- | Utility to reroute a request path to a file
rerouteTo :: String -> FilePath -> ActionM ()
rerouteTo dir = file . rerouteTo'
  where
    rerouteTo' ('/':path)     = rerouteTo' path
    rerouteTo' path
      | null (takeFileName path) = dir </> path </> "index.html"
      | otherwise                = dir </> path
