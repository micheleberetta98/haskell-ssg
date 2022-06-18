import           Test.Hspec

import           DocumentSpec
import           MacroSpec
import           ParserSpec

main = hspec $ do
  parserSpec
  macroSpec
  documentSpec

