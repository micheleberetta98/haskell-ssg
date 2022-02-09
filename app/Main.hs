{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Server
import           System.Environment


import qualified Data.Text.IO       as TIO
import           Document
import           Parser
import           ToHTML

main :: IO ()
main = do
  port <- getPort 4000
  serve "_build" port

getPort :: Int -> IO Int
getPort defaultPort = maybe defaultPort read <$> lookupEnv "PORT"

test :: IO ()
test = do
  Right l <- parseFile "_layouts/default.txt" layout
  Right d <- parseFile "_build/input.txt" document
  let Just out = toHTML <$> applyLayout d [("default", l)]
  TIO.writeFile "_build/out.html" out
