{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : FileModel
Description : Implements the file model for lexer and parser

This module is pretended to be imported qualified
or hiding Error.
-}
module FileModel (
  Range (),
  rangeStart,
  rangeEnd,
  Position (),
  absolutePosition,
  lineNumber,
  columnNumber,
  getLineStart,
  getLineEnd,
  getColumnEnd,
  getColumnStart,
  buildPosition,
  buildRange,
  Error (..),
) where

import qualified Data.Bifunctor as Bifunctor
import Data.List.NonEmpty (NonEmpty)
import Data.Natural (Natural)
import qualified Data.Text as Text
import qualified Validation

data Position = Position
  { absolutePosition :: Natural
  , lineNumber :: Natural
  , columnNumber :: Natural
  }
  deriving (Eq, Show)

instance Ord Position where
  p1 <= p2 = absolutePosition p1 <= absolutePosition p2

{- | The inequality
 >> absolutePosition rangeStart  <= absolutePosition rangeEnd
 must hold.
-}
data Range = Range
  { rangeStart :: Position
  , rangeEnd :: Position
  }
  deriving (Eq, Show)

data Error
  = NegativeNumberToPosition Text.Text Int
  | BadPositions Position Position
  deriving (Eq, Show)

getLineStart :: Range -> Natural
getLineStart Range{rangeStart = start} = lineNumber start

getLineEnd :: Range -> Natural
getLineEnd Range{rangeEnd = end} = lineNumber end

getColumnStart :: Range -> Natural
getColumnStart Range{rangeStart = start} = columnNumber start

getColumnEnd :: Range -> Natural
getColumnEnd Range{rangeEnd = end} = columnNumber end

-- | All parameters must be non negative integers.
buildPosition ::
  -- | absolutePosition
  Int ->
  -- | lineNumber
  Int ->
  -- | columnNumber
  Int ->
  Validation.Validation (NonEmpty Error) Position
buildPosition
  absolute
  line
  column =
    case test absolute "absolute"
      <> test line "line"
      <> test column "column" of
      Validation.Failure e -> Validation.Failure e
      Validation.Success _ ->
        Validation.Success
          Position
            { absolutePosition = makeNatural absolute
            , lineNumber = makeNatural line
            , columnNumber = makeNatural column
            }
   where
    test :: Int -> Text.Text -> Validation.Validation (NonEmpty Error) ()
    test value msg =
      Validation.failureIf
        (value < 0)
        $ NegativeNumberToPosition msg value

    makeNatural :: Int -> Natural
    makeNatural = fromInteger . toInteger

{- | First position must have greater absolutePosition than second
 or equal to success.
-}
buildRange ::
  Position ->
  Position ->
  Validation.Validation (NonEmpty Error) Range
buildRange p1 p2 =
  Bifunctor.second (const (Range{rangeStart = p1, rangeEnd = p2})) test
 where
  test :: Validation.Validation (NonEmpty Error) ()
  test =
    Validation.failureIf
      (absolutePosition p1 > absolutePosition p2)
      $ BadPositions p1 p2
