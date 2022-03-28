{-# LANGUAGE OverloadedStrings #-}

module LexerSpec (spec) where

import qualified Text.Megaparsec as Megaparsec

import qualified Lexer
import qualified Tokens

import Test.Hspec
import Test.Hspec.Megaparsec

import TextModel.Internal.TextModel (unsafeMkPosition, unsafeMkRange)

mkTokenWithRange :: Tokens.RealTokenKind -> Integer -> Integer -> Integer -> Integer -> Tokens.RealToken
mkTokenWithRange kind line1 column1 line2 column2 =
  Tokens.RealToken (unsafeMkRange (unsafeMkPosition line1 column1) (unsafeMkPosition line2 column2)) kind

spec :: Spec
spec = do
  describe "Lexer" $
    do
      context "Keywords" $ do
        it "let" $
          Megaparsec.parse Lexer.keywordLet "" "let"
            `shouldParse` mkTokenWithRange Tokens.Let 0 0 0 3
