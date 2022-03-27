module ParserSyntaxTree where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.Text as Text

import qualified FileModel

newtype ImportPath = ImportPath (NonEmpty Text.Text)

data WithRange a = WithRange {value :: a, range :: FileModel.Range}

data Tree
  = Import (WithRange ImportPath)
  | ImportUnqualified (WithRange ImportPath)
  | ImportAS (WithRange ImportPath) (WithRange ImportPath)
