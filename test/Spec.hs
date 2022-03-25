import Test.Hspec

import qualified FileModelSpec
import qualified LexerSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "FileModel" FileModelSpec.spec
  describe "Lexer" LexerSpec.spec
