module Server
  ( serve
  )
where

import           Web.Scotty

serve :: FilePath -> Int -> IO ()
serve dir port = scotty (fromIntegral port) $ do
  get "/" $ file (dir ++ "/index.html")
  get (regex "^/(.*\\.html)$") $ do
    path <- param "0"
    file (dir ++ path)
  get (regex "^/(.*)$") $ do
    path <- param "0"
    file (dir ++ path ++ "/index.html")
