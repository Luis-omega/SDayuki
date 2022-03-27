{-# LANGUAGE OverloadedStrings #-}

module Lexer where

import qualified Data.Text as Text
import qualified Text.Megaparsec as Megaparsec
import qualified Text.Megaparsec.Char as Megaparsec.Char

import qualified FileModel
import qualified Tokens

import Control.Monad (liftM2)
import Data.Char (isDigit, isLetter)
import Data.Functor (($>))
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (many, (<?>), (<|>))

type Lexer = Megaparsec.Parsec Void Text

spaces :: Lexer ()
spaces = Megaparsec.takeWhileP (Just "space") (== ' ') $> ()

getPosition :: Lexer FileModel.Position
getPosition =
  FileModel.positionFromMegaparsec <$> Megaparsec.getSourcePos

mkToken :: (a -> Tokens.RealTokenKind) -> Lexer a -> Lexer Tokens.RealToken
mkToken constructor toLex =
  do
    start <- getPosition
    content <- toLex
    end <- getPosition
    _ <- spaces
    return $
      Tokens.RealToken
        (FileModel.unsafeMkRange start end)
        (constructor content)

-- Let
keywordLet :: Lexer Tokens.RealToken
keywordLet =
  mkToken (const Tokens.Let) (Megaparsec.Char.string "let") <?> "Keyword(let)"

-- Identifier
identifierStart :: Lexer Text
identifierStart = Text.singleton <$> Megaparsec.Char.letterChar

identifierInnerCharPredicate :: Char -> Bool
identifierInnerCharPredicate c =
  isLetter c || isDigit c || c == '_'

identifier :: Lexer Text
identifier =
  let innerParser =
        Megaparsec.takeWhileP
          (Just "Identifier inner character")
          identifierInnerCharPredicate
   in liftM2 (<>) identifierStart innerParser <?> "identifier"

keywordIdentifier :: Lexer Tokens.RealToken
keywordIdentifier =
  mkToken Tokens.Identifier identifier <?> "Identifier"

-- Natural
asciiDigits :: Lexer Text.Text
asciiDigits =
  Megaparsec.takeWhile1P
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
  mkToken (const Tokens.NewLine) (Megaparsec.Char.char '\n')

-- Equal
keywordEqual :: Lexer Tokens.RealToken
keywordEqual =
  mkToken (const Tokens.Equal) (Megaparsec.Char.char '=')

-- In
keywordIn :: Lexer Tokens.RealToken
keywordIn =
  mkToken (const Tokens.In) (Megaparsec.Char.string "Keyword(in)")

-- TypeAnnotationStart
keywordTypeAnnotation :: Lexer Tokens.RealToken
keywordTypeAnnotation =
  mkToken (const Tokens.TypeAnnotationStart) (Megaparsec.Char.char ':') <?> "Type annotation (:)"

-- In
keywordArrow :: Lexer Tokens.RealToken
keywordArrow =
  mkToken (const Tokens.Arrow) (Megaparsec.Char.string "->")

-- ModuleAccess
keywordModuleAccess :: Lexer Tokens.RealToken
keywordModuleAccess =
  mkToken (const Tokens.ModuleAccess) (Megaparsec.Char.char '^') <?> "Module access(^)"

-- RecordAccess
keywordRecordAccess :: Lexer Tokens.RealToken
keywordRecordAccess =
  mkToken (const Tokens.RecordAccess) (Megaparsec.Char.char '.') <?> "Record access(.)"

-- Import
keywordImport :: Lexer Tokens.RealToken
keywordImport =
  mkToken (const Tokens.Import) (Megaparsec.Char.string "import") <?> "Keyword(import)"

-- UnQualified
keywordUnQualified :: Lexer Tokens.RealToken
keywordUnQualified =
  mkToken (const Tokens.UnQualified) (Megaparsec.Char.string "unqualified") <?> "Keyword(unqualified)"

-- As
keywordAs :: Lexer Tokens.RealToken
keywordAs =
  mkToken (const Tokens.UnQualified) (Megaparsec.Char.string "as") <?> "Keyword(as)"

-- Hole
hole :: Lexer Char
hole = Megaparsec.Char.char '_' <?> "hole start('_')"

keywordHole :: Lexer Tokens.RealToken
keywordHole =
  mkToken (const Tokens.Hole) (Megaparsec.Char.char '.') <?> "Anonymous hole(_)"

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
  mkToken (const Tokens.EOF) Megaparsec.eof <?> "EOF"

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
    <|> Megaparsec.try keywordHole
    <|> keywordEnumeratedHole
    <|> keywordEof

-- ToDo, consume initial spaces and newlines but
-- ensure lexing being at column 0
lexer :: Lexer [Tokens.RealToken]
lexer = many lexChar
