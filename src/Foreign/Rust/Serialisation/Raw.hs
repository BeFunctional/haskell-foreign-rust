-- | Dealing with types that are represented in raw form Haskell side
module Foreign.Rust.Serialisation.Raw (
    IsRaw(..)
  ) where

import Data.FixedSizeArray (FixedSizeArray)
import Data.Proxy
import Data.Word
import GHC.TypeLits

import qualified Data.ByteString      as Strict
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.FixedSizeArray  as FSA
import qualified Data.Vector.Generic  as Vector

{-------------------------------------------------------------------------------
  Abstract over raw representation
-------------------------------------------------------------------------------}

-- | Datatype that is represented as raw bytes Haskell-side
--
-- Sometimes when dealing with Rust-side values we represent them as opaque
-- values Haskell side: essentially just a list of bytes. However, we typically
-- still want to be able to display, serialise and deserialise such values.
--
-- 'IsRaw' abstracts over how exactly those raw bytes stored; in the following
-- modules we then provide deriving-via combinators for specific encodings:
--
-- * Foreign.Rust.Serialisation.Raw.Base16 (base-16, hexadecimal, hexdump)
-- * Foreign.Rust.Serialisation.Raw.Base58 (base-58, bitcoin format)
-- * Foreign.Rust.Serialisation.Raw.Base64 (base-64)
-- * Foreign.Rust.Serialisation.Raw.Decimal (list of decimal values)
--
-- All of these modules provide combinators to derive
--
-- * 'Prelude.Show' and 'Data.Structured.Show' (law-abiding in the sense of
--   generating valid Haskell)
-- * 'FromJSON' and 'ToJSON'
--
-- All of these modules show the raw bytes, they just differ in /how/ they show
-- those raw bytes. If you want a more human-readable format, consider using
-- "Foreign.Rust.Serialisation.JSON".
class IsRaw a where
  {-# MINIMAL (toRaw | toBytes), (fromRaw | fromBytes) #-}

  -- rawSize

  rawSize :: a -> Word32
  rawSize = fromIntegral . Lazy.length . toRaw

  -- toRaw

  toRaw :: a -> Lazy.ByteString
  toRaw = Lazy.pack . toBytes

  toBytes :: a -> [Word8]
  toBytes = Lazy.unpack . toRaw

  -- fromRaw

  fromRaw :: Lazy.ByteString -> Either String a
  fromRaw = fromBytes . Lazy.unpack

  fromBytes :: [Word8] -> Either String a
  fromBytes = fromRaw . Lazy.pack


{-------------------------------------------------------------------------------
  ByteString
-------------------------------------------------------------------------------}

instance IsRaw Lazy.ByteString where
  rawSize = fromIntegral . Lazy.length
  toRaw   = id
  fromRaw = Right

instance IsRaw Strict.ByteString where
  rawSize = fromIntegral . Strict.length
  toRaw   = Lazy.fromStrict
  fromRaw = Right . Lazy.toStrict

{-------------------------------------------------------------------------------
  [Word8]
-------------------------------------------------------------------------------}

instance IsRaw [Word8] where
  rawSize   = fromIntegral . length
  toBytes   = id
  fromBytes = Right

{-------------------------------------------------------------------------------
  FixedSizeArray
-------------------------------------------------------------------------------}

instance KnownNat n => IsRaw (FixedSizeArray n Word8) where
  rawSize   = const $ fromIntegral $ natVal (Proxy @n)
  toBytes   = Vector.toList
  fromBytes = \xs ->
      let expectedSize, actualSize :: Int
          expectedSize = fromIntegral $ natVal (Proxy @n)
          actualSize   = length xs
      in if actualSize == expectedSize
           then Right $ FSA.fromList xs
           else Left $ "Expected " ++ show expectedSize ++ "bytes, "
                    ++ "but got " ++ show actualSize ++ ": "
                    ++ show xs

