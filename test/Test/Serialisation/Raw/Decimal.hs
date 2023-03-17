{-# LANGUAGE TemplateHaskell #-}

module Test.Serialisation.Raw.Decimal (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Aeson as Aeson

import Test.Serialisation.Types
import Test.Util.TH

import Foreign.Rust.Serialisation.Raw.Decimal (asDecimal)

tests :: TestTree
tests = testGroup "Test.Serialisation.Raw.Decimal" [
      testCase "show"       test_show
    , testCase "structured" test_structured
    , testCase "json"       test_json
    ]

test_show :: Assertion
test_show =
    assertEqual "" exampleUsesDecimal $
      $(reparseShow exampleUsesDecimal)

test_structured :: Assertion
test_structured =
    assertEqual "" exampleUsesDecimal $
      $(reparseStructured exampleUsesDecimal)

test_json :: Assertion
test_json =
    assertEqual "" (Right exampleUsesDecimal) $
      Aeson.eitherDecode $ Aeson.encode exampleUsesDecimal