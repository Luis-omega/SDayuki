import Test.Hspec

import qualified LexerSpec
import qualified TextModelSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "TextModel" TextModelSpec.spec
  describe "Lexer" LexerSpec.spec
