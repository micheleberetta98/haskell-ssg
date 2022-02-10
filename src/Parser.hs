module Parser
  ( Parser
  , ParserError
  , document
  , macro
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

parseFile :: FilePath -> Parser a -> IO (Either ParserError a)
parseFile file p = TM.parse p file <$> T.readFile file

