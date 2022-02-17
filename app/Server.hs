{-# LANGUAGE OverloadedStrings #-}

module Server (serve) where

import           System.Environment
import           System.FilePath
import           Web.Scotty


serve :: FilePath -> IO ()
serve dir = do
  port <- getPort 4000
  server dir port

getPort :: Int -> IO Int
getPort defaultPort = maybe defaultPort read <$> lookupEnv "PORT"

server :: FilePath -> Int -> IO ()
server dir port = scotty (fromIntegral port) $ do
  get (regex "^/(.*)$") $
    param "0" >>= rerouteTo dir

rerouteTo :: String -> FilePath -> ActionM ()
rerouteTo dir = file . rerouteTo'
  where
    rerouteTo' ('/':path)     = rerouteTo' path
    rerouteTo' path
      | null (takeFileName path) = dir </> path </> "index.html"
      | otherwise                = dir </> path
