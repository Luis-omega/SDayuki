module FileModelSpec (spec) where

import qualified FileModel
import Test.Hspec
import Test.QuickCheck
import qualified Validation

genPosition :: Gen (Validation.Validation (Int, Int) FileModel.Position)
genPosition = do
  line <- arbitrary
  column <- arbitrary
  return $ case FileModel.buildPosition line column of
    Validation.Success a -> Validation.Success a
    Validation.Failure _ -> Validation.Failure (line, column)

checkPosition :: Validation.Validation (Int, Int) FileModel.Position -> Bool
checkPosition (Validation.Success pos) =
  FileModel.lineNumber pos >= 0
    && FileModel.columnNumber pos >= 0
checkPosition (Validation.Failure (line, column)) =
  line < 0 || column < 0

genRange :: Gen (Validation.Validation ((Int, Int), (Int, Int)) FileModel.Range)
genRange =
  do
    line1 <- nonNeg
    column1 <- nonNeg
    line2 <- nonNeg
    column2 <- nonNeg
    case FileModel.buildRange <$> FileModel.buildPosition line1 column1 <*> FileModel.buildPosition line2 column2 of
      Validation.Success (Validation.Success a) -> return $ Validation.Success a
      _ -> return $ Validation.Failure ((line1, column1), (line2, column2))
 where
  nonNeg :: Gen Int
  nonNeg =
    getNonNegative <$> arbitrary

checkRange :: Validation.Validation ((Int, Int), (Int, Int)) FileModel.Range -> Bool
checkRange (Validation.Success range) =
  FileModel.rangeStart range
    <= FileModel.rangeEnd range
checkRange (Validation.Failure (start, end)) = start > end

spec :: Spec
spec = do
  describe "FileModel" $
    do
      context "Position" $ do
        it "buildPosition" $
          forAll genPosition checkPosition
      context "Range" $ do
        it "buildRange" $
          forAll genRange checkRange
