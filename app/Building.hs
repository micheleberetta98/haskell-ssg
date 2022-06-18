{-|
  Module      : Building
  Description : Creation of output data

  This modules applies macros and layouts in order to obtain renderable data.
-}
module Building
  ( BuildError
  , buildFiles
  , build
  , renderFile
  )
where

import           Control.Exception
import           Control.Monad
import           Data.Bifunctor
import           Data.List
import           Document
import           File
import           Macro
import           ToHtml

------------ Types and instances

-- | Some errors can appear when building files, such as a layout or a 'Macro' missing
newtype BuildError = NoLayoutFound FilePath

-- | A list of 'BuildError'
newtype BuildErrors = BuildErrors [BuildError]
  deriving (Show)

instance Exception BuildErrors

instance Show BuildError where
  show (NoLayoutFound p) = concat ["(!) Something went wrong at ", p, ": maybe the layout doesn't exist?"]

------------ Building files

buildFiles :: ([Layout], [Macro], [File Document]) -> IO [File [Content]]
buildFiles (layouts, macros, docs) = do
  let (buildErrors, finalDocs) = separateErrors $ map (build layouts macros) docs
  unless (null buildErrors) $
    throw (BuildErrors buildErrors)
  pure finalDocs

-- | Applies a layout (i.e. a 'Macro') to a 'Document'
build :: [Layout] -> [Macro] -> File Document -> Either BuildError (File [Content])
build ls ms (File p doc) = File p <$> applyMacros doc
  where
    applyMacros = toEither . fmap (expandAll ms) . applyLayout ls
    toEither (Just x) = Right x
    toEither Nothing  = Left (NoLayoutFound p)

-- | Writes the HTML of a list of 'Content' into a specific directory
renderFile :: FilePath -> File [Content] -> IO ()
renderFile dir (File path stuff) = save dir "html" (File path (render $ toHtml stuff))

------------ Utilities

separateErrors :: [Either a b] -> ([a], [b])
separateErrors = bimap reverse reverse . foldl' accum ([], [])
  where
    accum (es, xs) (Left e)  = (e : es, xs)
    accum (es, xs) (Right x) = (es, x : xs)
