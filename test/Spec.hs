import           Test.Hspec

import           DocumentSpec (documentSpec)
import           MacroSpec    (macroSpec)
import           ParserSpec   (parserSpec)

main = hspec $ do
  parserSpec
  macroSpec
  documentSpec

