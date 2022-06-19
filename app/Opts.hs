{-|
  Module      : Opts
  Description : CLI options management

  Here is defined the 'Options' type, which includes all command line arguments
  that specify all the necessary folders.
-}
module Opts
  ( Options(..)
  , getOpts
  , defaultOpts
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

defaultOpts :: Options
defaultOpts = Options
  { srcFolder = "_src"
  , layoutsFolder = "_layouts"
  , macrosFolder = "_macros"
  , buildFolder = "_build"
  , staticFolder = "_static"
  , outputStaticFolder = "_build/static"
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
      <> value (layoutsFolder defaultOpts)
      <> help "Folder with layout macros"
      )
  <*> strOption
      (  long "macros"
      <> short 'm'
      <> metavar "MACROS_FOLDER"
      <> value (macrosFolder defaultOpts)
      <> help "Folder with generic macros, not to be used as layouts"
      )
  <*> strOption
      (  long "static"
      <> short 's'
      <> metavar "STATIC_FOLDER"
      <> value (staticFolder defaultOpts)
      <> help "Folder with static files"
      )
  <*> strOption
      (  long "output"
      <> short 'o'
      <> metavar "OUT_FOLDER"
      <> value (buildFolder defaultOpts)
      <> help "Output folder"
      )
  <*> strOption
      (  long "out-static"
      <> metavar "OUT_STATIC_FOLDER"
      <> value (outputStaticFolder defaultOpts)
      <> help "Output static files' folder"
      )
