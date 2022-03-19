module FileModelSpec (spec) where

import qualified FileModel
import Test.Hspec
import Test.QuickCheck
import qualified Validation

genPosition :: Gen (Validation.Validation (Int, Int, Int) FileModel.Position)
genPosition = do
  absolute <- arbitrary
  line <- arbitrary
  column <- arbitrary
  return $ case FileModel.buildPosition absolute line column of
    Validation.Success a -> Validation.Success a
    Validation.Failure _ -> Validation.Failure (absolute, line, column)

checkPosition :: Validation.Validation (Int, Int, Int) FileModel.Position -> Bool
checkPosition (Validation.Success pos) =
  FileModel.absolutePosition pos >= 0
    && FileModel.lineNumber pos >= 0
    && FileModel.columnNumber pos >= 0
checkPosition (Validation.Failure (absolute, line, column)) =
  absolute < 0 || line < 0 || column < 0

genRange :: Gen (Validation.Validation (Int, Int) FileModel.Range)
genRange =
  do
    absolute1 <- nonNeg
    line1 <- nonNeg
    column1 <- nonNeg
    absolute2 <- nonNeg
    line2 <- nonNeg
    column2 <- nonNeg
    case FileModel.buildRange <$> FileModel.buildPosition absolute1 line1 column1 <*> FileModel.buildPosition absolute2 line2 column2 of
      Validation.Success (Validation.Success a) -> return $ Validation.Success a
      _ -> return $ Validation.Failure (absolute1, absolute2)
 where
  nonNeg :: Gen Int
  nonNeg =
    getNonNegative <$> arbitrary

checkRange :: Validation.Validation (Int, Int) FileModel.Range -> Bool
checkRange (Validation.Success range) =
  FileModel.absolutePosition (FileModel.rangeStart range)
    <= FileModel.absolutePosition (FileModel.rangeEnd range)
checkRange (Validation.Failure (abs1, abs2)) = abs1 > abs2

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
