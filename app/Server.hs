{-# LANGUAGE OverloadedStrings #-}

module Server (serve) where

import           System.Environment
import           System.FilePath
import           Web.Scotty

-- | Starts the server that redirects all requests to @dir@.
-- The port is read from the env variable PORT (default 4000).
serve :: FilePath -> IO ()
serve dir = do
  port <- getPort 4000
  server dir port

-- | Reads the env variable PORT
getPort :: Int -> IO Int
getPort defaultPort = maybe defaultPort read <$> lookupEnv "PORT"

-- | A simple server that maps requests to a specific file in a directory
server :: FilePath -> Int -> IO ()
server dir port = scotty (fromIntegral port) $ do
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
