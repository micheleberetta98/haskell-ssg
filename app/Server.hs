{-# LANGUAGE OverloadedStrings #-}

module Server (serve) where

import           System.FilePath
import           Web.Scotty

serve :: FilePath -> Int -> IO ()
serve dir port = scotty (fromIntegral port) $ do
  get "^/(*)" $
    param "0" >>= file . rerouteTo dir

rerouteTo :: String -> FilePath -> FilePath
rerouteTo dir path
  | null (takeFileName path) = replaceFileName path' "index.html"
  | otherwise                = path'
  where path' = replaceDirectory path dir
