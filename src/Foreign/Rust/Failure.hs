module Foreign.Rust.Failure (
    Failure -- Opaque
  , failureMessage
  , mkFailure
  , throwFailure
  , throwFailureIO
  ) where

import Control.Exception
import Data.Text (Text)
import GHC.Stack

-- | Failure reported by a Rust function
--
-- TODO: For some cases we might be able to attach a Rust callstack, too.
data Failure = Failure {
      failureMessage :: Text
    , failureCallstackHaskell :: PrettyCallStack
    }
  deriving stock (Show)
  deriving anyclass (Exception)

mkFailure :: HasCallStack => Text -> Failure
mkFailure e = Failure e (PrettyCallStack callStack)

newtype PrettyCallStack = PrettyCallStack CallStack

instance Show PrettyCallStack where
  show (PrettyCallStack stack) = prettyCallStack stack

throwFailure :: Either Failure a -> a
throwFailure (Left err) = throw err
throwFailure (Right a)  = a

throwFailureIO :: Either Failure a -> IO a
throwFailureIO (Left err) = throwIO err
throwFailureIO (Right a)  = return a