{-# LANGUAGE OverloadedStrings #-}

-- | External (Rust-side) JSON serialisation/deserialisation
--
-- Intended for qualified import.
--
-- > import Foreign.Rust.External.JSON (UseExternalJSON, ShowAsJSON)
-- > import qualified Foreign.Rust.External.JSON as External
module Foreign.Rust.External.JSON (
    -- * Serialisation
    JSON(..)
  , ToJSON(..)
  , FromJSON(..)
    -- * Deriving-via: derive Aeson instances using external (de)serialiser
  , UseExternalJSON(..)
  ) where

import Codec.Borsh
import Foreign.Rust.Failure
import GHC.Stack

import qualified Data.Aeson           as Aeson
import qualified Data.Aeson.Encoding  as Aeson (unsafeToEncoding)
import qualified Data.Aeson.Types     as Aeson (parseFail)
import qualified Data.Binary.Builder  as Binary
import qualified Data.ByteString.Lazy as Lazy (ByteString)

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

-- | Serialised JSON
newtype JSON = JSON Lazy.ByteString
  deriving stock (Eq)
  deriving newtype (BorshSize, ToBorsh, FromBorsh)

-- | Types with a Rust-side JSON renderer
class ToJSON a where
  toJSON :: a -> JSON

-- | Types with a Rust-side JSON parser
class FromJSON a where
  fromJSON :: HasCallStack => JSON -> Either Failure a

{-------------------------------------------------------------------------------
  Deriving-via: derive Aeson instances using external (de)serialiser
-------------------------------------------------------------------------------}

newtype UseExternalJSON a = UseExternalJSON a

instance ToJSON a => Aeson.ToJSON (UseExternalJSON a) where
  toJSON (UseExternalJSON x) =
      reparse (toJSON x)
    where
      -- We get serialised JSON from the external renderer, and then need to
      -- re-parse that to a 'Value'. If this fails, however, it would mean that
      -- the Rust-side generated invalid JSON.
      reparse :: JSON -> Aeson.Value
      reparse (JSON bs) =
          case Aeson.eitherDecode bs of
            Left  err -> error err
            Right val -> val

  -- This relies on 'toJSON' generating valid JSON
  toEncoding (UseExternalJSON x) =
      case toJSON x of
        JSON bs -> Aeson.unsafeToEncoding $ Binary.fromLazyByteString bs

instance FromJSON a => Aeson.FromJSON (UseExternalJSON a) where
  parseJSON val =
      case fromJSON (JSON (Aeson.encode val)) of
        Left failure -> Aeson.parseFail (show failure)
        Right tx     -> return $ UseExternalJSON tx

