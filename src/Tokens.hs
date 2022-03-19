{-# LANGUAGE DuplicateRecordFields #-}

{- |
Module      : Tokens
Description : The definition for lexer and parser

This module is pretended to be imported qualified
-}
module Tokens where

import qualified Data.Text as Text
import qualified FileModel

data RealTokenKind
  = Let
  | Variable Text.Text
  | NewLine
  | Equal
  | In
  | TypeAnnotationStart
  | Arrow

data RealToken = RealToken
  { tokenRange :: FileModel.Range
  , tokenKind :: RealTokenKind
  , realTokenText :: Text.Text
  }

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
