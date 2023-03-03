{-# LANGUAGE OverloadedStrings #-}
-- make unsafePerformIO safer
{-# OPTIONS_GHC -fno-cse -fno-full-laziness #-}

module Main (main) where

import LispVal ( LispVal(String, Nil, Atom, List, Bool, Number) )
import Parser ( readExpr )
import Eval
    ( basicEnv,
      fileToEvalForm,
      getFileContents,
      runASTinEnv,
      textToEvalForm )

import qualified Data.Text as T

import Test.Hspec ( hspec, describe, it, shouldBe, SpecWith )
import System.IO.Unsafe ( unsafePerformIO )

main :: IO ()
main = do
  hspec $ describe "src/Parser.hs" $ do
    it "Atom" $
      readExpr "bb-8?" `shouldBe` Right (Atom "bb-8?")
    it "Num Negative" $
      readExpr "-2187" `shouldBe` Right (Number (-2187))
    it "Num Positive" $
      readExpr "112233" `shouldBe` Right (Number 112233)
    it "Num Positive with Sign" $
      readExpr "+12345" `shouldBe` Right (Number 12345)
    it "String" $
      readExpr "\"Gen L Organa\"" `shouldBe` Right (String "Gen L Organa")
    it "Bool True" $
      readExpr "#t" `shouldBe` Right (Bool True)
    it "Bool False" $
      readExpr "#f" `shouldBe` Right (Bool False)
