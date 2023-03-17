-- | Base-64 encoding
--
-- See "Foreign.Rust.Serialisation.Raw" for discussion.
module Foreign.Rust.Serialisation.Raw.Base64 (
    -- * Deriving-via support
    AsBase64(..)
  ) where

import Control.Monad
import Data.Aeson (ToJSON(..), FromJSON(..), withText)
import Data.Annotated
import Data.String

import qualified Data.Aeson.Types            as Aeson
import qualified Data.ByteString.Base64.Lazy as Base64
import qualified Data.ByteString.Lazy        as Lazy
import qualified Data.ByteString.Lazy.Char8  as Char8
import qualified Data.Structured             as Structured
import qualified Data.Text                   as Text

import Foreign.Rust.Serialisation.Raw

{-------------------------------------------------------------------------------
  Deriving-via combinator
-------------------------------------------------------------------------------}

-- | Serialise using base-64
--
-- The 'Show' instance will produce something like
--
-- > "AQIDBA=="
--
-- This depends on @OverloadedStrings@.
--
-- NOTE: 'Annotated' instance is only useful when using 'AsBase64' directly
-- (rather than using deriving-via).
newtype AsBase64 a = AsBase64 { unwrapAsBase64 :: a }
  deriving newtype CanAnnotate

{-------------------------------------------------------------------------------
  JSON
-------------------------------------------------------------------------------}

instance IsRaw a => ToJSON (AsBase64 a) where
  toJSON = toJSON  . encode . unwrapAsBase64

instance IsRaw a => FromJSON (AsBase64 a) where
  parseJSON = either Aeson.parseFail (return . AsBase64) . decode <=< parseJSON

{-------------------------------------------------------------------------------
  Show
-------------------------------------------------------------------------------}

instance IsRaw a => Show (AsBase64 a) where
  show = show . encode . unwrapAsBase64

instance IsRaw a => Structured.Show (AsBase64 a) where
  toValue = Structured.toValue . encode . unwrapAsBase64

instance IsRaw a => IsString (AsBase64 a) where
  fromString = either error AsBase64 . decode . fromString

{-------------------------------------------------------------------------------
  Auxiliary: base-64 encoded value
-------------------------------------------------------------------------------}

newtype Base64 = Base64 { getBase64 :: Lazy.ByteString }
  deriving newtype (Show, IsString)

instance ToJSON Base64 where
  toJSON = Aeson.String . Text.pack . Char8.unpack . getBase64

instance FromJSON Base64 where
  parseJSON = withText "Base64" $ return . Base64 . Char8.pack . Text.unpack

instance Structured.Show Base64 where
  toValue = Structured.String

encode :: IsRaw a => a -> Base64
encode = Base64 . Base64.encode . toRaw

decode :: IsRaw a => Base64 -> Either String a
decode = fromRaw <=< Base64.decode . getBase64
