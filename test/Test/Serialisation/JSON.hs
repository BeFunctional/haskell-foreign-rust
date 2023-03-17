{-# LANGUAGE TemplateHaskell #-}

module Test.Serialisation.JSON (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Test.Serialisation.Types
import Test.Util.TH

import Foreign.Rust.Serialisation.JSON (asJSON)

tests :: TestTree
tests = testGroup "Test.Serialisation.JSON" [
      testCase "show"       test_show
    , testCase "structured" test_structured
    ]

test_show :: Assertion
test_show =
    assertEqual "" exampleUsesJSON $
      $(reparseShow exampleUsesJSON)

test_structured :: Assertion
test_structured =
    assertEqual "" exampleUsesJSON $
      $(reparseStructured exampleUsesJSON)
