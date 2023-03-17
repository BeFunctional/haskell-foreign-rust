module Foreign.Rust.Marshall.Util (
    -- * Borsh
    serialiseStrict
  , deserialiseStrictOrPanic
  , deserialiseLazyOrPanic
    -- * Casting
  , castFromSigned
  , castToSigned
  , castFromSignedLen
  ) where

import Codec.Borsh
import Data.Typeable
import Foreign
import Foreign.C
import GHC.Stack

import qualified Data.ByteString      as Strict
import qualified Data.ByteString.Lazy as Lazy

{-------------------------------------------------------------------------------
  Borsh
-------------------------------------------------------------------------------}

serialiseStrict :: ToBorsh a => a -> Strict.ByteString
serialiseStrict = Lazy.toStrict . serialiseBorsh

deserialiseStrictOrPanic ::
      (HasCallStack, FromBorsh a, Typeable a)
   => Strict.ByteString -> a
deserialiseStrictOrPanic = deserialiseLazyOrPanic . Lazy.fromStrict

deserialiseLazyOrPanic :: forall a.
     (HasCallStack, FromBorsh a, Typeable a)
  => Lazy.ByteString -> a
deserialiseLazyOrPanic bs =
    case deserialiseBorsh bs of
      Right a  -> a
      Left err -> error $ concat [
          "deserialiseLazyOrPanic for " ++ show (typeOf (Proxy @a)) ++ ": "
        , show err ++ "\n"
        , "buffer: " ++ show (Lazy.unpack bs)
        , " (" ++ show (Lazy.length bs) ++ ")"
        ]

{-------------------------------------------------------------------------------
  Casting
-------------------------------------------------------------------------------}

castFromSigned :: Ptr CChar -> Ptr CUChar
castFromSigned = castPtr

castToSigned :: Ptr CUChar -> Ptr CChar
castToSigned = castPtr

castFromSignedLen :: (Ptr CChar, Int) -> (Ptr CUChar, CULong)
castFromSignedLen (ptr, len) = (castFromSigned ptr, fromIntegral len)

