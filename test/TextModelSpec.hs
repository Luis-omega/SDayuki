module TextModelSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import qualified TextModel.TextModel as TextModel
import qualified Validation

genPosition :: Gen (Validation.Validation (Int, Int) TextModel.Position)
genPosition = do
  line <- arbitrary
  column <- arbitrary
  return $ case TextModel.buildPosition line column of
    Validation.Success a -> Validation.Success a
    Validation.Failure _ -> Validation.Failure (line, column)

checkPosition :: Validation.Validation (Int, Int) TextModel.Position -> Bool
checkPosition (Validation.Success pos) =
  TextModel.getPositionLine pos >= 0
    && TextModel.getPositionColumn pos >= 0
checkPosition (Validation.Failure (line, column)) =
  line < 0 || column < 0

genRange :: Gen (Validation.Validation ((Int, Int), (Int, Int)) TextModel.Range)
genRange =
  do
    line1 <- nonNeg
    column1 <- nonNeg
    line2 <- nonNeg
    column2 <- nonNeg
    case TextModel.buildRange <$> TextModel.buildPosition line1 column1 <*> TextModel.buildPosition line2 column2 of
      Validation.Success (Validation.Success a) -> return $ Validation.Success a
      _ -> return $ Validation.Failure ((line1, column1), (line2, column2))
 where
  nonNeg :: Gen Int
  nonNeg =
    getNonNegative <$> arbitrary

checkRange :: Validation.Validation ((Int, Int), (Int, Int)) TextModel.Range -> Bool
checkRange (Validation.Success range) =
  TextModel.getRangeStart range
    <= TextModel.getRangeEnd range
checkRange (Validation.Failure (start, end)) = start > end

spec :: Spec
spec = do
  describe "TextModel" $
    do
      context "Position" $ do
        it "buildPosition" $
          forAll genPosition checkPosition
      context "Range" $ do
        it "buildRange" $
          forAll genRange checkRange
