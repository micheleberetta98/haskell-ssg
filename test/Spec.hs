import           Test.Hspec

import           MacroSpec
import           ParserSpec

main = hspec $ do
  parserSpec
  macroSpec

