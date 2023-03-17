{-# LANGUAGE OverloadedStrings #-}

-- | List of decimal values
--
-- See "Foreign.Rust.Serialisation.Raw" for discussion.
module Foreign.Rust.Serialisation.Raw.Decimal (
    -- * Deriving-via support
    AsDecimal(..)
    -- * Show instance
  , asDecimal
  ) where

import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.Typeable
import Data.Word

import qualified Data.Structured as Structured

import Foreign.Rust.Serialisation.Raw

{-------------------------------------------------------------------------------
  Deriving-via combinator
-------------------------------------------------------------------------------}

-- | Serialise to list of decimal values
--
-- The 'Show' instance will produce something like
--
-- > asDecimal @MyType [1,2,3,4]
--
-- This depends on 'asDecimal' (defined in this module).
newtype AsDecimal a = AsDecimal { unwrapAsDecimal :: a }

{-------------------------------------------------------------------------------
  JSON
-------------------------------------------------------------------------------}

instance IsRaw a => ToJSON (AsDecimal a) where
  toJSON = toJSON . toBytes . unwrapAsDecimal

instance IsRaw a => FromJSON (AsDecimal a) where
  parseJSON = either parseFail (return . AsDecimal) . fromBytes <=< parseJSON

{-------------------------------------------------------------------------------
  Show
-------------------------------------------------------------------------------}

deriving
  via Structured.ToPreludeShow (AsDecimal a)
  instance (Typeable a, IsRaw a) => Show (AsDecimal a)

instance (Typeable a, IsRaw a) => Structured.Show (AsDecimal a) where
  toValue (AsDecimal x) =
      Structured.Constr "asDecimal" [typeRep (Proxy @a)] [
          Structured.toValue (toBytes x)
        ]

asDecimal :: IsRaw a => [Word8] -> a
asDecimal = either error id . fromBytes