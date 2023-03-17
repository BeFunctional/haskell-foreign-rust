-- | Base-58 encoding
--
-- See "Foreign.Rust.Serialisation.Raw" for discussion.
module Foreign.Rust.Serialisation.Raw.Base58 (
    -- * Deriving-via support
    AsBase58(..)
  ) where

import Control.Monad
import Data.Aeson (ToJSON(..), FromJSON(..), withText)
import Data.Annotated
import Data.String

import qualified Data.Aeson.Types       as Aeson
import qualified Data.ByteString        as Strict
import qualified Data.ByteString.Base58 as Base58
import qualified Data.ByteString.Char8  as Char8
import qualified Data.ByteString.Lazy   as Lazy
import qualified Data.Structured        as Structured
import qualified Data.Text              as Text

import Foreign.Rust.Serialisation.Raw

{-------------------------------------------------------------------------------
  Deriving-via combinator
-------------------------------------------------------------------------------}

-- | Serialise using base-58
--
-- The 'Show' instance will produce something like
--
-- > "2VfUX"
--
-- This depends on @OverloadedStrings@.
--
-- NOTE: 'Annotated' instance is only useful when using 'AsBase58' directly
-- (rather than using deriving-via).
newtype AsBase58 a = AsBase58 { unwrapAsBase58 :: a }
  deriving newtype CanAnnotate

{-------------------------------------------------------------------------------
  JSON
-------------------------------------------------------------------------------}

instance IsRaw a => ToJSON (AsBase58 a) where
  toJSON = toJSON  . encode . unwrapAsBase58

instance IsRaw a => FromJSON (AsBase58 a) where
  parseJSON = either Aeson.parseFail (return . AsBase58) . decode <=< parseJSON

{-------------------------------------------------------------------------------
  Show
-------------------------------------------------------------------------------}

instance IsRaw a => Show (AsBase58 a) where
  show = show . encode . unwrapAsBase58

instance IsRaw a => Structured.Show (AsBase58 a) where
  toValue = Structured.toValue . encode . unwrapAsBase58

instance IsRaw a => IsString (AsBase58 a) where
  fromString = either error AsBase58 . decode . fromString

{-------------------------------------------------------------------------------
  Auxiliary: base-58 encoded value
-------------------------------------------------------------------------------}

-- | Base58-encoded value
--
-- NOTE: base-58 is a relatively expensive encoding; in particular, unlike
-- base-64, base-58 does not make it very easy to process chunks of data
-- separately. Ideally, it should therefore only be used for a small pieces of
-- data. For this reason, we use a strict bytestring here.
newtype Base58 = Base58 { getBase58 :: Strict.ByteString }
  deriving newtype (Show, IsString)

instance ToJSON Base58 where
  toJSON = Aeson.String . Text.pack . Char8.unpack . getBase58

instance FromJSON Base58 where
  parseJSON = withText "Base58" $ return . Base58 . Char8.pack . Text.unpack

instance Structured.Show Base58 where
  toValue = Structured.String

encode :: IsRaw a => a -> Base58
encode = Base58 . encodeBitcoin . Lazy.toStrict . toRaw

decode :: IsRaw a => Base58 -> Either String a
decode = fromRaw <=< fmap Lazy.fromStrict . decodeBitcoin . getBase58

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

encodeBitcoin :: Strict.ByteString -> Strict.ByteString
encodeBitcoin = Base58.encodeBase58 Base58.bitcoinAlphabet

decodeBitcoin :: Strict.ByteString -> Either String Strict.ByteString
decodeBitcoin =
      maybe (Left "invalid Base58 encoding") Right
    . Base58.decodeBase58 Base58.bitcoinAlphabet
