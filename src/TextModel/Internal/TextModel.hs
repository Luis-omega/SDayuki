{- |
Module      : TextModel.Internal.TextModel
Description : Implements the file model for lexer and parser

This module is pretended to be imported qualified
or hiding Error.
-}
module TextModel.Internal.TextModel (
  Range (..),
  Position (..),
  positionFromMegaparsec,
  unsafeMkRange,
  unsafeMkPosition,
  Error (..),
) where

import Data.Natural (Natural)
import qualified Data.Text as Text

import qualified Text.Megaparsec.Pos as Megaparsec

data Position = Position
  { lineNumber :: Natural
  , columnNumber :: Natural
  }
  deriving (Eq, Show)

instance Ord Position where
  p1 <= p2 = (lineNumber p1, columnNumber p1) <= (lineNumber p2, columnNumber p2)

{- | The inequality
 >> rangeStart <= rangeEnd
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

positionFromMegaparsec :: Megaparsec.SourcePos -> Position
positionFromMegaparsec (Megaparsec.SourcePos _ line column) =
  Position (convert line) (convert column)
 where
  convert :: Megaparsec.Pos -> Natural
  convert x = (fromInteger . toInteger) (Megaparsec.unPos x -1)

unsafeMkRange :: Position -> Position -> Range
unsafeMkRange = Range

unsafeMkPosition :: Integer -> Integer -> Position
unsafeMkPosition x y = Position (fromInteger x) (fromInteger y)
