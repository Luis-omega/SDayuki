import Test.Hspec

import qualified FileModelSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Foo" FileModelSpec.spec
