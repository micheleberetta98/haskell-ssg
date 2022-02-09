{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Server
import           System.Environment

main :: IO ()
main = do
  port <- getPort 4000
  serve "_build" port

getPort :: Int -> IO Int
getPort defaultPort = maybe defaultPort read <$> lookupEnv "PORT"

