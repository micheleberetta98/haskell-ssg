module Server
  ( serve
  )
where

import           Web.Scotty

serve :: FilePath -> Int -> IO ()
serve dir port = scotty (fromIntegral port) $ do
  get "/" $ file (dir ++ "/index.html")
