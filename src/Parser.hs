module Parser
  ( document
  , layout
  , parseFile
  )
where

import           Data.Text       (Text)
import qualified Data.Text       as T
import qualified Data.Text.IO    as T
import           Data.Void       (Void)
import           Parser.Internal
import           Text.Megaparsec (ParseErrorBundle)
import qualified Text.Megaparsec as TM

parseFile :: FilePath -> Parser a -> IO (Either (ParseErrorBundle Text Void) a)
parseFile file p = TM.parse p file <$> T.readFile file

