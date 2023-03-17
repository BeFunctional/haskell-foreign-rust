{-# LANGUAGE TemplateHaskell #-}

module Test.Serialisation.Raw.Base64 (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Aeson as Aeson

import Test.Serialisation.Types
import Test.Util.TH

tests :: TestTree
tests = testGroup "Test.Serialisation.Raw.Base64" [
      testCase "show"       test_show
    , testCase "structured" test_structured
    , testCase "json"       test_json
    ]

test_show :: Assertion
test_show =
    assertEqual "" exampleUsesBase64 $
      $(reparseShow exampleUsesBase64)

test_structured :: Assertion
test_structured =
    assertEqual "" exampleUsesBase64 $
      $(reparseStructured exampleUsesBase64)

test_json :: Assertion
test_json =
    assertEqual "" (Right exampleUsesBase64) $
      Aeson.eitherDecode $ Aeson.encode exampleUsesBase64