-- | Base-16 encoding (hexdump)
--
-- See "Foreign.Rust.Serialisation.Raw" for discussion.
module Foreign.Rust.Serialisation.Raw.Base16 (
    -- * Deriving-via support
    AsBase16(..)
  ) where

import Control.Monad
import Data.Aeson (ToJSON(..), FromJSON(..), withText)
import Data.Annotated
import Data.String

import qualified Data.Aeson.Types            as Aeson
import qualified Data.ByteString.Base16.Lazy as Base16
import qualified Data.ByteString.Lazy        as Lazy
import qualified Data.ByteString.Lazy.Char8  as Char8
import qualified Data.Structured             as Structured
import qualified Data.Text                   as Text

import Foreign.Rust.Serialisation.Raw

{-------------------------------------------------------------------------------
  Deriving-via combinator
-------------------------------------------------------------------------------}

-- | Show values in base-16 (hexadecimal/hexdump)
--
-- The 'Show' instance will produce something like
--
-- > "01020304"
--
-- This depends on @OverloadedStrings@.
--
-- NOTE: 'Annotated' instance is only useful when using 'AsBase16' directly
-- (rather than using deriving-via).
newtype AsBase16 a = AsBase16 { unwrapAsBase16 ::a }
  deriving newtype CanAnnotate

{-------------------------------------------------------------------------------
  JSON
-------------------------------------------------------------------------------}

instance IsRaw a => ToJSON (AsBase16 a) where
  toJSON = toJSON  . encode . unwrapAsBase16

instance IsRaw a => FromJSON (AsBase16 a) where
  parseJSON = either Aeson.parseFail (return . AsBase16) . decode <=< parseJSON

{-------------------------------------------------------------------------------
  Show
-------------------------------------------------------------------------------}

instance IsRaw a => Show (AsBase16 a) where
  show = show . encode . unwrapAsBase16

instance IsRaw a => Structured.Show (AsBase16 a) where
  toValue = Structured.toValue . encode . unwrapAsBase16

instance IsRaw a => IsString (AsBase16 a) where
  fromString = either error AsBase16 . decode . fromString

{-------------------------------------------------------------------------------
  Auxiliary: base-16 encoded value
-------------------------------------------------------------------------------}

newtype Base16 = Base16 { getBase16 :: Lazy.ByteString }
  deriving newtype (Show, IsString)

instance ToJSON Base16 where
  toJSON = Aeson.String . Text.pack . Char8.unpack . getBase16

instance FromJSON Base16 where
  parseJSON = withText "Base16" $ return . Base16 . Char8.pack . Text.unpack

instance Structured.Show Base16 where
  toValue = Structured.String

encode :: IsRaw a => a -> Base16
encode = Base16 . Base16.encode . toRaw

decode :: IsRaw a => Base16 -> Either String a
decode = fromRaw <=< Base16.decode . getBase16
