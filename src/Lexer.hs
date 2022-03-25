{-# LANGUAGE OverloadedStrings #-}

module Lexer where

import qualified Data.Text as Text
import qualified FileModel
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Tokens

import Data.Char (isDigit, isLetter)
import Data.Text (Text)

import Control.Applicative
import Control.Monad.State.Strict
import Data.Functor
import Data.Void
import Text.Megaparsec hiding (State, many, some)
import Text.Megaparsec.Char hiding (space)

type Lexer = Parsec Void Text

sc :: Lexer ()
sc =
  L.space
    space1
    empty
    empty

spaces = takeWhileP (Just "space") (== ' ') $> ()

testChar :: Lexer Char
testChar =
  do
    init <- getPosition
    out <- char '_'
    end <- getPosition
    _ <- spaces
    return out

getPosition :: Lexer FileModel.Position
getPosition =
  FileModel.positionFromMegaparsec <$> getSourcePos

mkToken :: (a -> Tokens.RealTokenKind) -> Lexer a -> Lexer Tokens.RealToken
mkToken constructor lexer =
  do
    start <- getPosition
    content <- lexer
    end <- getPosition
    _ <- spaces
    return $
      Tokens.RealToken
        (FileModel.unsafeMkRange start end)
        (constructor content)

-- Let
keywordLet :: Lexer Tokens.RealToken
keywordLet =
  mkToken (const Tokens.Let) (string "let") <?> "Keyword(let)"

-- Identifier
identifierStart :: Lexer Text
identifierStart = Text.singleton <$> letterChar

identifierInnerCharPredicate :: Char -> Bool
identifierInnerCharPredicate c =
  isLetter c || isDigit c || c == '_'

identifier :: Lexer Text
identifier =
  let innerParser =
        takeWhileP
          (Just "Identifier inner character")
          identifierInnerCharPredicate
   in liftM2 (<>) identifierStart innerParser <?> "identifier"

keywordIdentifier :: Lexer Tokens.RealToken
keywordIdentifier =
  mkToken Tokens.Identifier identifier <?> "Identifier"

-- Natural
asciiDigits :: Lexer Text.Text
asciiDigits =
  takeWhile1P
    (Just "ASCII digit")
    isDigit

keywordNatural :: Lexer Tokens.RealToken
keywordNatural =
  mkToken
    Tokens.NaturalToken
    (read . Text.unpack <$> asciiDigits)
    <?> "Identifier"

-- NewLine
keywordNewLine :: Lexer Tokens.RealToken
keywordNewLine =
  mkToken (const Tokens.NewLine) (char '\n')

-- Equal
keywordEqual :: Lexer Tokens.RealToken
keywordEqual =
  mkToken (const Tokens.Equal) (char '=')

-- In
keywordIn :: Lexer Tokens.RealToken
keywordIn =
  mkToken (const Tokens.In) (string "Keyword(in)")

-- TypeAnnotationStart
keywordTypeAnnotation :: Lexer Tokens.RealToken
keywordTypeAnnotation =
  mkToken (const Tokens.TypeAnnotationStart) (char ':') <?> "Type annotation (:)"

-- In
keywordArrow :: Lexer Tokens.RealToken
keywordArrow =
  mkToken (const Tokens.Arrow) (string "->")

-- ModuleAccess
keywordModuleAccess :: Lexer Tokens.RealToken
keywordModuleAccess =
  mkToken (const Tokens.ModuleAccess) (char '^') <?> "Module access(^)"

-- RecordAccess
keywordRecordAccess :: Lexer Tokens.RealToken
keywordRecordAccess =
  mkToken (const Tokens.RecordAccess) (char '.') <?> "Record access(.)"

-- Hole
hole :: Lexer Char
hole = char '_' <?> "hole start('_')"

keywordHole :: Lexer Tokens.RealToken
keywordHole =
  mkToken (const Tokens.Hole) (char '.') <?> "Anonymous hole(_)"

-- EnumeratedHole
enumeratedHole :: Lexer Int
enumeratedHole =
  hole *> (read . Text.unpack <$> asciiDigits)

keywordEnumeratedHole :: Lexer Tokens.RealToken
keywordEnumeratedHole =
  mkToken Tokens.EnumeratedHole enumeratedHole <?> "Enumerated hole(_number)"

-- EOF
keywordEof :: Lexer Tokens.RealToken
keywordEof =
  mkToken (const Tokens.EOF) eof <?> "EOF"

lexChar :: Lexer Tokens.RealToken
lexChar =
  keywordLet
    <|> keywordIdentifier
    <|> keywordNatural
    <|> keywordNewLine
    <|> keywordEqual
    <|> keywordIn
    <|> keywordTypeAnnotation
    <|> keywordArrow
    <|> keywordModuleAccess
    <|> keywordRecordAccess
    <|> try keywordHole
    <|> keywordEnumeratedHole
    <|> keywordEof

-- ToDo, consume initial spaces and newlines but
-- ensure lexing being at column 0
lexer :: Lexer [Tokens.RealToken]
lexer = many lexChar
