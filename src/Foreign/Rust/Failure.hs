module Foreign.Rust.Failure (
    Failure -- Opaque
  , failureMessage
  , mkFailure
  ) where

import GHC.Stack
import Data.Text (Text)

-- | Failure reported by a Rust function
--
-- TODO: For some cases we might be able to attach a Rust callstack, too.
data Failure = Failure {
      failureMessage :: Text
    , failureCallstackHaskell :: PrettyCallStack
    }
  deriving (Show)

mkFailure :: HasCallStack => Text -> Failure
mkFailure e = Failure e (PrettyCallStack callStack)

newtype PrettyCallStack = PrettyCallStack CallStack

instance Show PrettyCallStack where
  show (PrettyCallStack stack) = prettyCallStack stack


