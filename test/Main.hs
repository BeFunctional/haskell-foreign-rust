module Main (main) where

import Test.Tasty

import qualified Test.Serialisation.JSON
import qualified Test.Serialisation.Raw.Base16
import qualified Test.Serialisation.Raw.Base58
import qualified Test.Serialisation.Raw.Base64
import qualified Test.Serialisation.Raw.Decimal

main :: IO ()
main = defaultMain $ testGroup "foreign-rust" [
      Test.Serialisation.JSON.tests
    , Test.Serialisation.Raw.Base16.tests
    , Test.Serialisation.Raw.Base58.tests
    , Test.Serialisation.Raw.Base64.tests
    , Test.Serialisation.Raw.Decimal.tests
    ]