{-# LANGUAGE DuplicateRecordFields #-}

{- |
Module      : Tokens
Description : The definition for lexer and parser

This module is pretended to be imported qualified
-}
module Tokens where

import Data.Natural (Natural)
import qualified Data.Text as Text
import qualified FileModel

data RealTokenKind
  = Let
  | Identifier Text.Text
  | NaturalToken Natural
  | NewLine
  | Equal
  | In
  | TypeAnnotationStart
  | Arrow
  | ModuleAccess
  | RecordAccess
  | Import
  | UnQualified
  | As
  | Hole
  | EnumeratedHole Int
  | EOF
  deriving (Eq)

instance Show RealTokenKind where
  show Let = "let"
  show (Identifier name) = Text.unpack name
  show (NaturalToken value) = show value
  show NewLine = "\\n"
  show Equal = "="
  show In = "in"
  show TypeAnnotationStart = ":"
  show Arrow = "->"
  show ModuleAccess = "^"
  show RecordAccess = "."
  show Hole = "_"
  show (EnumeratedHole value) = "_" <> show value
  show EOF = "EOF"

data RealToken = RealToken
  { tokenRange :: FileModel.Range
  , tokenKind :: RealTokenKind
  }
  deriving (Eq)

instance Show RealToken where
  show (RealToken range kind) = "Token( " <> show range <> " , " <> show kind <> " )"

data GeneratedTokenKind
  = LetIndentationStart
  | LetSeparator
  | LetIndentationEnd
  | InIndentationStart
  | InIndentationEnd

data GeneratedToken = GeneratedToken
  { tokenRange :: FileModel.Range
  , tokenKind :: GeneratedTokenKind
  }

data Token
  = RealTokenWrapper
      RealToken
  | GeneratedTokenWrapper
      GeneratedToken
