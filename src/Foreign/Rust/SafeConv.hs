module Foreign.Rust.SafeConv (
    SafeConv(..)
  ) where

import Data.Word
import Foreign.C.Types

class SafeConv a b where
  safeConvFrom :: a -> b
  safeConvTo   :: b -> a

instance SafeConv CULong Word64 where
  safeConvFrom = fromIntegral
  safeConvTo   = fromIntegral

instance SafeConv CULLong Word64 where
  safeConvFrom = fromIntegral
  safeConvTo   = fromIntegral
