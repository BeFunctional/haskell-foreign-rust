-- | Marshall from a Rust-side allocated buffer
--
-- Intended for unqualified import.
module Foreign.Rust.Marshall.External (
    fromExternalBorsh
  ) where

import Codec.Borsh
import Control.Exception
import Data.Typeable
import Data.Word
import Foreign.C
import Foreign.Concurrent
import Foreign.Ptr

import qualified Data.ByteString.Internal as Strict

import Foreign.Rust.Marshall.Util

data External

foreign import ccall "haskell_ffi_external_ptr"
     externalPtr
  :: Ptr External -> Ptr Word8

foreign import ccall "haskell_ffi_external_len"
     externalLen
  :: Ptr External -> CSize

foreign import ccall "haskell_ffi_external_free"
     externalFree
  :: Ptr External -> IO ()

-- | Internal auxiliary: cast pointer
--
-- For ease of integration with c2hs, 'fromExternalBorsh' takes a @Ptr ()@ as
-- input instead of the more accurate @Ptr External@.
castToExternal :: Ptr () -> Ptr External
castToExternal = castPtr

-- | Output marshaller for values stored in Rust-allocated buffer
--
-- Should be used together with the Rust function @marshall_to_haskell_external@
-- (from @haskell-ffi@).
fromExternalBorsh :: (FromBorsh a, Typeable a) => Ptr () -> IO a
fromExternalBorsh (castToExternal -> ptr) = do
    len  <- evaluate $ fromIntegral $ externalLen ptr
    fptr <- newForeignPtr (externalPtr ptr) (externalFree ptr)

    let bs :: Strict.ByteString
        bs = Strict.PS fptr 0 len

    return $ deserialiseStrictOrPanic bs
