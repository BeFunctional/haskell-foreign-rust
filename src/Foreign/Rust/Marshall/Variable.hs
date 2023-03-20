{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Marshalling to and from Rust, using Borsh
--
-- This module deals with types with variable sized encodings.
-- See also "Foreign.Rust.Marshall.Fixed".
module Foreign.Rust.Marshall.Variable (
    -- * Haskell to Rust
    toBorshVar
    -- * Rust to Haskell
  , Buffer -- opaque
  , getVarBuffer
  , withBorshVarBuffer
  , withBorshMaxBuffer
  , withBorshFailure
    -- ** Pure variants
  , withPureBorshVarBuffer
  , withPureBorshMaxBuffer
  , withPureBorshFailure
  ) where

import Codec.Borsh
import Data.Bifunctor
import Data.Text (Text)
import Data.Typeable
import Foreign
import Foreign.C.Types
import GHC.Stack
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.ByteString as Strict

import Foreign.Rust.Marshall.Util
import Foreign.Rust.Failure

{-------------------------------------------------------------------------------
  Haskell to Rust
-------------------------------------------------------------------------------}

toBorshVar ::
     (ToBorsh a, StaticBorshSize a ~ 'HasVariableSize)
  => a -> ((Ptr CUChar, CULong) -> IO r) -> IO r
toBorshVar a k =
    Strict.useAsCStringLen (serialiseStrict a) (k . castFromSignedLen)

{-------------------------------------------------------------------------------
  Rust to Haskell
-------------------------------------------------------------------------------}

-- | Buffer containing value of (phantom) type @a@
data Buffer a = Buffer (Ptr CUChar) (Ptr CULong)

getVarBuffer :: Buffer a -> (Ptr CUChar, Ptr CULong)
getVarBuffer (Buffer buf ptrSize) = (buf, ptrSize)

-- | Provide buffer for foreign call
--
-- We start with an initial buffer of 1 kB. If that suffices, we copy the
-- (appropriate part of) the buffer to a ByteString and we're done. If not,
-- the foreign call will tell us what the required buffer size is, so we
-- try again with a larger buffer.
--
-- We allocate these buffers on the Haskell heap ('allocaBytes'), not the C
-- heap ('malloc'). This ensures that these buffers are visible to Haskell
-- profiling tools, and also appears to be more reliable on OSX. A slight
-- downside is that it doesn't give us a way to /change/ the size of a buffer,
-- but that's not an essential feature.
withBorshVarBuffer :: forall a.
     ( FromBorsh a
     , StaticBorshSize a ~ 'HasVariableSize
     , Typeable a
     )
  => (Buffer a -> IO ()) -> IO a
withBorshVarBuffer = withBorshBufferOfInitSize 1024

withBorshMaxBuffer :: forall a.
     ( FromBorsh a
     , StaticBorshSize a ~ 'HasVariableSize
     , BorshMaxSize a
     , Typeable a
     )
  => (Buffer a -> IO ()) -> IO a
withBorshMaxBuffer =
   withBorshBufferOfInitSize initBufSize
  where
    initBufSize :: CULong
    initBufSize = fromIntegral $ borshMaxSize (Proxy @a)

-- | Wrapper around 'withBorshVarBuffer' with explicit support for failures
withBorshFailure :: forall a.
     ( FromBorsh a
     , StaticBorshSize a ~ 'HasVariableSize
     , Typeable a
     , HasCallStack
     )
  => (Buffer (Either Text a) -> IO ()) -> IO (Either Failure a)
withBorshFailure = fmap (first mkFailure) . withBorshVarBuffer

{-------------------------------------------------------------------------------
  Pure variants
-------------------------------------------------------------------------------}

withPureBorshVarBuffer :: forall a.
     ( FromBorsh a
     , StaticBorshSize a ~ 'HasVariableSize
     , Typeable a
     )
  => (Buffer a -> IO ()) -> a
withPureBorshVarBuffer = unsafePerformIO . withBorshVarBuffer

withPureBorshMaxBuffer :: forall a.
     ( FromBorsh a
     , StaticBorshSize a ~ 'HasVariableSize
     , BorshMaxSize a
     , Typeable a
     )
  => (Buffer a -> IO ()) -> a
withPureBorshMaxBuffer = unsafePerformIO . withBorshMaxBuffer

withPureBorshFailure :: forall a.
     ( FromBorsh a
     , StaticBorshSize a ~ 'HasVariableSize
     , Typeable a
     , HasCallStack
     )
  => (Buffer (Either Text a) -> IO ()) -> Either Failure a
withPureBorshFailure = unsafePerformIO . withBorshFailure


{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

-- | Generalization of 'withBorshVarBuffer' and 'withMaxBorshBuffer'
withBorshBufferOfInitSize :: forall a.
     ( FromBorsh a
     , StaticBorshSize a ~ 'HasVariableSize
     , Typeable a
     )
  => CULong -> (Buffer a -> IO ()) -> IO a
withBorshBufferOfInitSize initBufSize f = do
    mFirstAttempt <- allocaBytes (culongToInt initBufSize) $ \buf -> do
      (bigEnough, reqSz) <- callWithSize buf initBufSize
      if bigEnough then
        Right . deserialiseStrictOrPanic <$>
          Strict.packCStringLen (castPtr buf, culongToInt reqSz)
      else
        return $ Left reqSz
    case mFirstAttempt of
      Right r ->
        return r
      Left reqSz -> do
        allocaBytes (culongToInt reqSz) $ \buf -> do
          (bigEnough, reqSz') <- callWithSize buf reqSz
          if bigEnough && reqSz == reqSz' then
            deserialiseStrictOrPanic <$>
              Strict.packCStringLen (castPtr buf, culongToInt reqSz)
          else
            fail $ concat [
                "withBorshVarBuffer: unexpected change in required buffer size. "
              , "was " ++ show reqSz  ++ ", "
              , "now " ++ show reqSz' ++ "."
              ]
  where
    -- Call the function with the current buffer size
    -- Returns whether or not the buffer was big enough, and the required size
    callWithSize :: Ptr CUChar -> CULong -> IO (Bool, CULong)
    callWithSize buf providedSize = alloca $ \ptrBufSize -> do
        poke ptrBufSize providedSize
        f $ Buffer buf ptrBufSize
        requiredSize <- peek ptrBufSize
        return (requiredSize <= providedSize, requiredSize)

    -- Buffer allocations should not take a signed 'Int' as argument 🙄
    culongToInt :: CULong -> Int
    culongToInt = fromIntegral
