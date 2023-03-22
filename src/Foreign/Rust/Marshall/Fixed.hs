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
  , allocMaxBuffer
  , fromBorsh
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
  Rust to Haskell: exact size known
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

allocMaxBuffer :: forall a.
     ( BorshSize a
     , StaticBorshSize a ~ 'HasVariableSize
     , BorshMaxSize a
     )
  => ((Ptr CUChar, CULong) -> IO a) -> IO a
allocMaxBuffer k =
    let n = borshMaxSize (Proxy @a)
    in allocaBytes (cast n) $ \ptr -> k (ptr, fromIntegral n)
  where
    cast :: Word32 -> Int
    cast = fromIntegral

fromBorsh :: (FromBorsh a, Typeable a) => Ptr CUChar -> CULong -> IO a
fromBorsh ptr len =
    deserialiseStrictOrPanic <$>
      Strict.packCStringLen (castToSigned ptr, fromIntegral len)

