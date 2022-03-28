module Parser where

import qualified Data.Text as Text
import qualified Text.Megaparsec as Megaparsec
import qualified Text.Megaparsec.Char as Megaparsec.Char

import qualified Lexer
import qualified TextModel.TextModel as TextModel
import qualified Tokens

import Control.Monad.Combinators.NonEmpty (sepBy1)
import Data.Char (isDigit, isLetter)
import Data.Text (Text)
import Data.Void (Void)

import qualified ParserSyntaxTree as Pst

type Parser = Megaparsec.Parsec Void Text

-- Fix this, identifiers must be forbid to have spaces between them
-- but using the keyword need a partial function to get the inner text
--importPath :: Parser Pst.ImportPath
--importPath =
--  Pst.ImportPath <$> sepBy1 Lexer.keywordIdentifier Lexer.keywordModuleAccess
