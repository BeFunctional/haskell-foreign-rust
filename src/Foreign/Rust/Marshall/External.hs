-- | Marshall from a Rust-side allocated buffer
--
-- Intended for unqualified import.
module Foreign.Rust.Marshall.External (
    fromExternalBorsh
  ) where

import Codec.Borsh
import Data.Typeable
import Data.Word
import Foreign.C
import Foreign.Ptr

import qualified Data.ByteString.Internal as Strict

import Foreign.Rust.Marshall.Util
import Foreign.ForeignPtr

data ExternalBuffer

{-------------------------------------------------------------------------------
  Foreign imports

  Although 'externalPtr' and 'externalLen' are morally pure, we make
  them live in IO to make reasoning about order of operations easier in
  'fromExternalBorsh'.

  These C functions are defined in the companion Rust @haskell-ffi@ library.
-------------------------------------------------------------------------------}

foreign import ccall unsafe "haskell_ffi_external_ptr"
     externalPtr
  :: Ptr ExternalBuffer -> IO (Ptr Word8)

foreign import ccall unsafe "haskell_ffi_external_len"
     externalLen
  :: Ptr ExternalBuffer -> IO CSize

foreign import ccall unsafe "&haskell_ffi_external_free_env"
     externalFree
  :: FinalizerEnvPtr ExternalBuffer Word8

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

-- | Cast pointer
--
-- For ease of integration with c2hs, 'fromExternalBorsh' takes a @Ptr ()@ as
-- input instead of the more accurate @Ptr ExternalBuffer@.
castToExternal :: Ptr () -> Ptr ExternalBuffer
castToExternal = castPtr

{-------------------------------------------------------------------------------
  Public API
-------------------------------------------------------------------------------}

-- | Output marshaller for values stored in Rust-allocated buffer
--
-- Should be used together with the Rust function @marshall_to_haskell_external@
-- (from @haskell-ffi@).
fromExternalBorsh :: (FromBorsh a, Typeable a) => Ptr () -> IO a
fromExternalBorsh (castToExternal -> vec) = do
    ptr  <- externalPtr vec
    len  <- fromIntegral <$> externalLen vec
    fptr <- newForeignPtrEnv externalFree vec ptr

    let bs :: Strict.ByteString
        bs = Strict.fromForeignPtr fptr 0 len

    return $ deserialiseStrictOrPanic bs

