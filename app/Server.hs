{-# LANGUAGE OverloadedStrings #-}

module Server where

import           System.FilePath
import           Web.Scotty

serve :: FilePath -> Int -> IO ()
serve dir port = scotty (fromIntegral port) $ do
  get (regex "^/(.*)$") $
    param "0" >>= rerouteTo dir

rerouteTo :: String -> FilePath -> ActionM ()
rerouteTo dir = file . rerouteTo'
  where
    rerouteTo' ('/':path)     = rerouteTo' path
    rerouteTo' path
      | null (takeFileName path) = dir </> path </> "index.html"
      | otherwise                = dir </> path
