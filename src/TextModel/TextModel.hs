{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : TextModel.TextModel
Description : Implements the file model for lexer and parser

This module is pretended to be imported qualified
or hiding Error.
-}
module TextModel.TextModel (
  Position (),
  Range (),
  getPositionLine,
  getPositionColumn,
  getLineStart,
  getLineEnd,
  getColumnEnd,
  getColumnStart,
  getRangeStart,
  getRangeEnd,
  buildPosition,
  buildRange,
) where

import qualified Data.Bifunctor as Bifunctor
import qualified Data.Text as Text
import qualified Validation

import Data.List.NonEmpty (NonEmpty)
import Data.Natural (Natural)

import TextModel.Internal.TextModel

-- | All parameters must be non negative integers.
buildPosition ::
  -- | lineNumber
  Int ->
  -- | columnNumber
  Int ->
  Validation.Validation (NonEmpty Error) Position
buildPosition
  line
  column =
    case test line "line"
      <> test column "column" of
      Validation.Failure e -> Validation.Failure e
      Validation.Success _ ->
        Validation.Success
          Position
            { lineNumber = makeNatural line
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

-- | First position must be lower or equal than second position
buildRange ::
  Position ->
  Position ->
  Validation.Validation (NonEmpty Error) Range
buildRange p1 p2 =
  Bifunctor.second (const (Range{rangeStart = p1, rangeEnd = p2})) test
 where
  test :: Validation.Validation (NonEmpty Error) ()
  test =
    Validation.failureUnless
      (p1 <= p2)
      $ BadPositions p1 p2

getPositionLine :: Position -> Natural
getPositionLine = lineNumber

getPositionColumn :: Position -> Natural
getPositionColumn = columnNumber

getLineStart :: Range -> Natural
getLineStart Range{rangeStart = start} = lineNumber start

getLineEnd :: Range -> Natural
getLineEnd Range{rangeEnd = end} = lineNumber end

getColumnStart :: Range -> Natural
getColumnStart Range{rangeStart = start} = columnNumber start

getColumnEnd :: Range -> Natural
getColumnEnd Range{rangeEnd = end} = columnNumber end

getRangeStart :: Range -> Position
getRangeStart = rangeStart

getRangeEnd :: Range -> Position
getRangeEnd = rangeEnd
