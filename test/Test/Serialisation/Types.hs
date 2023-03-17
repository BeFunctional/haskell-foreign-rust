{-# LANGUAGE QuasiQuotes #-}

-- | Example types
--
-- Defined as separate module to avoid TH stage restrictions
module Test.Serialisation.Types (
    UsesDecimal(..)
  , exampleUsesDecimal
  , UsesBase16(..)
  , exampleUsesBase16
  , UsesBase58(..)
  , exampleUsesBase58
  , UsesBase64(..)
  , exampleUsesBase64
  , UsesJSON(..)
  , exampleUsesJSON
  ) where

import Data.Aeson
import Data.Aeson.QQ.Simple
import Data.String
import Data.Word

import qualified Data.Structured as Structured

import Foreign.Rust.Serialisation.JSON
import Foreign.Rust.Serialisation.Raw
import Foreign.Rust.Serialisation.Raw.Base16
import Foreign.Rust.Serialisation.Raw.Base58
import Foreign.Rust.Serialisation.Raw.Base64
import Foreign.Rust.Serialisation.Raw.Decimal

{-------------------------------------------------------------------------------
  AsDecimal
-------------------------------------------------------------------------------}

newtype UsesDecimal = UsesDecimal [Word8]
  deriving stock (Eq)
  deriving newtype (IsRaw)
  deriving (Show, Structured.Show) via AsDecimal UsesDecimal
  deriving (FromJSON, ToJSON) via AsDecimal UsesDecimal

exampleUsesDecimal :: UsesDecimal
exampleUsesDecimal = UsesDecimal [1, 2, 3, 4]

{-------------------------------------------------------------------------------
  AsBase16
-------------------------------------------------------------------------------}

newtype UsesBase16 = UsesBase16 [Word8]
  deriving stock (Eq)
  deriving newtype (IsRaw)
  deriving (Show, Structured.Show, IsString) via AsBase16 UsesBase16
  deriving (FromJSON, ToJSON) via AsBase16 UsesBase16

exampleUsesBase16 :: UsesBase16
exampleUsesBase16 = UsesBase16 [1, 2, 3, 4]

{-------------------------------------------------------------------------------
  AsBase58
-------------------------------------------------------------------------------}

newtype UsesBase58 = UsesBase58 [Word8]
  deriving stock (Eq)
  deriving newtype (IsRaw)
  deriving (Show, Structured.Show, IsString) via AsBase58 UsesBase58
  deriving (FromJSON, ToJSON) via AsBase58 UsesBase58

exampleUsesBase58 :: UsesBase58
exampleUsesBase58 = UsesBase58 [1, 2, 3, 4]

{-------------------------------------------------------------------------------
  AsBase64
-------------------------------------------------------------------------------}

newtype UsesBase64 = UsesBase64 [Word8]
  deriving stock (Eq)
  deriving newtype (IsRaw)
  deriving (Show, Structured.Show, IsString) via AsBase64 UsesBase64
  deriving (FromJSON, ToJSON) via AsBase64 UsesBase64

exampleUsesBase64 :: UsesBase64
exampleUsesBase64 = UsesBase64 [1, 2, 3, 4]

{-------------------------------------------------------------------------------
  AsJSON
-------------------------------------------------------------------------------}

newtype UsesJSON = UsesJSON Value
  deriving stock (Eq)
  deriving newtype (ToJSON, FromJSON)
  deriving (Show, Structured.Show) via AsJSON UsesJSON

exampleUsesJSON :: UsesJSON
exampleUsesJSON = UsesJSON [aesonQQ|
    { "a": null
    , "b": [1, 2, 3]
    }
  |]
