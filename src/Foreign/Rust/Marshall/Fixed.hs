{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Marshalling to and from Rust, using Borsh
--
-- This module deals with types with fixed sized encodings.
-- See also "Foreign.Rust.Marshall.Variable".
module Foreign.Rust.Marshall.Fixed (
    -- * Haskell to Rust
    toBorshFixed
    -- * Rust to Haskell
  , allocFixedBuffer
  , fromBorshFixed
  ) where

import Codec.Borsh
import Data.Proxy
import Data.Typeable (Typeable)
import Foreign
import Foreign.C.Types

import qualified Data.ByteString as Strict

import Foreign.Rust.Marshall.Util

{-------------------------------------------------------------------------------
  Haskell to Rust
-------------------------------------------------------------------------------}

toBorshFixed ::
     (ToBorsh a, StaticBorshSize a ~ 'HasKnownSize)
  => a -> ((Ptr CUChar, CULong) -> IO r) -> IO r
toBorshFixed a k =
    Strict.useAsCStringLen (serialiseStrict a) (k . castFromSignedLen)

{-------------------------------------------------------------------------------
  Rust to Haskell
-------------------------------------------------------------------------------}

allocFixedBuffer :: forall a.
     (BorshSize a, StaticBorshSize a ~ 'HasKnownSize)
  => ((Ptr CUChar, CULong) -> IO a) -> IO a
allocFixedBuffer k =
    case borshSize (Proxy @a) of
      SizeKnown n ->
        allocaBytes (cast n) $ \ptr -> k (ptr, fromIntegral n)
  where
    cast :: Word32 -> Int
    cast = fromIntegral

fromBorshFixed ::
     (FromBorsh a, StaticBorshSize a ~ 'HasKnownSize, Typeable a)
  => Ptr CUChar -> CULong -> IO a
fromBorshFixed ptr len =
    deserialiseStrictOrPanic <$>
      Strict.packCStringLen (castToSigned ptr, fromIntegral len)
