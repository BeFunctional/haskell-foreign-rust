-- | External (Rust-side) Bincode serialisation/deserialisation
--
-- Intended for qualified import.
--
-- > import qualified Foreign.Rust.External.Bincode as External
module Foreign.Rust.External.Bincode (
    -- * Serialisation
    Bincode(..)
  , ToBincode(..)
  , FromBincode(..)
  ) where

import Codec.Borsh

import qualified Data.ByteString.Lazy as Lazy

import Foreign.Rust.Serialisation.Raw

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

newtype Bincode = Bincode Lazy.ByteString
  deriving stock (Eq)
  deriving newtype (BorshSize, ToBorsh, FromBorsh)
  deriving newtype (IsRaw)

-- | Types with an external Bincode serialiser (typically, in Rust)
class ToBincode a where
  toBincode :: a -> Bincode

-- | Types with an external Bincode deserialiser (typically, in Rust)
class FromBincode a where
  fromBincode :: Bincode -> Either String a