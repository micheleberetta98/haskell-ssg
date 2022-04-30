module Opts
  ( Options(..)
  , getOpts
  ) where

import           Options.Applicative

data Options = Options
  { srcFolder          :: FilePath
  , layoutsFolder      :: FilePath
  , macrosFolder       :: FilePath
  , staticFolder       :: FilePath
  , buildFolder        :: FilePath
  , outputStaticFolder :: FilePath
  }

getOpts :: IO Options
getOpts = execParser opts
  where
    opts = info (options <**> helper)
          (  fullDesc
          <> progDesc "A Static Site Generator written in Haskell"
          <> header   "Haskell SSG"
          )

options :: Parser Options
options = Options
  <$> argument str (metavar "SRC" <> help "The source folder containing your files")
  <*> strOption
      (  long "layouts"
      <> short 'l'
      <> metavar "LAYOUTS_FOLDER"
      <> value "_layouts"
      <> help "Folder with layout macros"
      )
  <*> strOption
      (  long "macros"
      <> short 'm'
      <> metavar "MACROS_FOLDER"
      <> value "_macros"
      <> help "Folder with generic macros, not to be used as layouts"
      )
  <*> strOption
      (  long "static"
      <> short 's'
      <> metavar "STATIC_FOLDER"
      <> value "_static"
      <> help "Folder with static files"
      )
  <*> strOption
      (  long "output"
      <> short 'o'
      <> metavar "OUT_FOLDER"
      <> value "_build"
      <> help "Output folder"
      )
  <*> strOption
      (  long "out-static"
      <> metavar "OUT_STATIC_FOLDER"
      <> value "_build/static"
      <> help "Output static files' folder"
      )
