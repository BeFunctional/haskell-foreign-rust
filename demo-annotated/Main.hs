{-# LANGUAGE TemplateHaskell #-}

import Data.Annotated
import Data.Kind
import GHC.TypeLits

import qualified GHC.Generics as GHC
import qualified Generics.SOP as SOP

import qualified Data.Structured    as Structured
import qualified Data.Structured.TH as Structured

{-------------------------------------------------------------------------------
  Demonstration of annotations
-------------------------------------------------------------------------------}

--
-- Suppose we have some data type that is opaque Haskell-side (just some bytes),
--

data Keypair = Keypair
  deriving stock (Show, GHC.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
  deriving anyclass (Structured.Show)
  deriving CanAnnotate via PairWithAnnotation Keypair

--
-- Perhaps we can inspect this datatype using an FFI
--

data Pubkey = Pubkey
  deriving stock (Show, GHC.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
  deriving anyclass (Structured.Show)

data Secret = Secret
  deriving stock (Show, GHC.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
  deriving anyclass (Structured.Show)

keypairPubkey :: Keypair -> Pubkey
keypairPubkey Keypair = Pubkey

keypairSecret :: Keypair -> Secret
keypairSecret Keypair = Secret

--
-- When we show a Keypair, we'd like to annotate it with these derived values
--

data KeypairAnnotation = KeypairAnnotation {
      pubkey :: Pubkey
    , secret :: Secret
    }
  deriving stock (Show, GHC.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
  deriving anyclass (Structured.Show)

type instance Annotation Keypair = KeypairAnnotation

instance ComputeAnnotation Keypair where
  computeAnnotation kp = KeypairAnnotation {
        pubkey = keypairPubkey kp
      , secret = keypairSecret kp
      }

{-------------------------------------------------------------------------------
  Generics
-------------------------------------------------------------------------------}

data RecordA = RecordA {
      recA_field1 :: Bool
    , recA_field2 :: Int
    }
  deriving stock (GHC.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
  deriving anyclass (Structured.Show)

-- A type, perhaps externally defined, with only standard Show instance
data SomeOtherType = SomeOtherType String
  deriving (Show)

data RecordB = RecordB {
      recB :: SomeOtherType
    }
  deriving stock (GHC.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

instance Structured.Show RecordB where
  toValue = Structured.gtoValueAfter Structured.FromPreludeShow

{-------------------------------------------------------------------------------
  Demonstrate TH support (crucically, with support for GADTs)
-------------------------------------------------------------------------------}

data SimpleEnum = SimpleEnumA | SimpleEnumB

Structured.deriveInstance 'SimpleEnumA [t|
     forall. Structured.Show SimpleEnum
  |]

data SimpleStruct a = SimpleStruct a Int

Structured.deriveInstance 'SimpleStruct [t|
    forall a. Structured.Show a => Structured.Show (SimpleStruct a)
  |]

data SomeGADT :: Symbol -> Type where
  Foo :: SomeGADT "foo"
  Bar :: SomeGADT "bar"

Structured.deriveInstance 'Foo [t|
    forall k. Structured.Show (SomeGADT k)
  |]

class Foo a where
  data SomeAssocType a :: Type

instance Foo Int where
  data SomeAssocType Int = SomeInt Int

Structured.deriveInstance 'SomeInt [t|
    forall. Structured.Show (SomeAssocType Int)
  |]

data SomeRecord = SomeRecord {
      field1 :: Int
    , field2 :: Bool
    }

Structured.deriveInstance 'SomeRecord [t|
    forall. Structured.Show SomeRecord
  |]

{-------------------------------------------------------------------------------
  Main
-------------------------------------------------------------------------------}

main :: IO ()
main = do
    -- Annotations
    Structured.print . annotate $
      [(Just Keypair, True)]
    Structured.print . dropAnnotation @[(Maybe Keypair, Bool)] . annotate $
      [(Just Keypair, True)]
    -- Generics
    Structured.print $ RecordA { recA_field1 = True, recA_field2 = 5 }
    Structured.print $ RecordB { recB = SomeOtherType "hi" }
    -- TH
    Structured.print $ SimpleEnumA
    Structured.print $ SimpleStruct True 5
    Structured.print $ Foo
    Structured.print $ SomeInt 5
    Structured.print $ SomeRecord { field1 = 1, field2 = True }