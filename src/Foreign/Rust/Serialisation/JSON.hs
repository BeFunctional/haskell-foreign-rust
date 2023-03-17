{-# LANGUAGE OverloadedStrings #-}

-- | Serialise opaque types using JSON
--
-- See "Foreign.Rust.Serialisation.Raw" for detailed discussion.
module Foreign.Rust.Serialisation.JSON (
    -- * Deriving-via support
    AsJSON(..)
    -- * Show instance
  , asJSON
  ) where

import Data.Annotated
import Data.Aeson (FromJSON(..))
import Data.Typeable

import qualified Data.Aeson.Types as Aeson
import qualified Data.Structured  as Structured

{-------------------------------------------------------------------------------
  Deriving-via combinator
-------------------------------------------------------------------------------}

-- | Serialise using JSON
--
-- The 'Show' instance will produce something like
--
-- > asJSON @UsesJSON
-- >   [aesonQQ|
-- >     {
-- >       "a": null
-- >     , "b": [1,2,3]
-- >     }
-- >   |]
--
-- This depends on 'asJSON' (defined in this module), @QuasiQuotes@ and
-- "Data.Aeson.QQ.Simple".
--
-- NOTE: 'Annotated' instance is only useful when using 'AsBase64' directly
-- (rather than using deriving-via).
newtype AsJSON a = AsJSON { unwrapAsJSON :: a }
  deriving newtype CanAnnotate

{-------------------------------------------------------------------------------
  Show
-------------------------------------------------------------------------------}

deriving
  via Structured.ToPreludeShow (AsJSON a)
  instance (Typeable a, Aeson.ToJSON a) => Show (AsJSON a)

instance (Typeable a, Aeson.ToJSON a) => Structured.Show (AsJSON a) where
  toValue (AsJSON x) =
      Structured.Constr "asJSON" [typeRep (Proxy @a)] [
          Structured.JSON (Aeson.toJSON x)
        ]

asJSON :: forall a. FromJSON a => Aeson.Value -> a
asJSON = either error id . Aeson.parseEither Aeson.parseJSON
