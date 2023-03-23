{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableSuperClasses #-}
module Data.Annotated (
    -- * Definition
    CanAnnotate(..)
    -- * Deriving-via support
    -- ** Computing annotations
  , Annotation
  , PairWithAnnotation(..)
  , ComputeAnnotation(..)
  , WithAnnotation(..)
    -- ** Other combinators
  , NoAnnotation(..)
  , AnnotateFoldable(..)
  , AnnotateGenericallyAs(..)
  ) where

import Data.Functor.Identity
import Data.Int
import Data.Kind
import Data.Map (Map)
import Data.Proxy
import Data.SOP (SOP)
import Data.Tuple.Solo
import Data.WideWord
import Data.Word

import qualified Data.Aeson          as Aeson
import qualified Data.SOP.Constraint as SOP
import qualified Generics.SOP        as SOP
import qualified GHC.Generics        as GHC

import qualified Data.Structured as Structured

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

class CanAnnotate a where
  type Annotated a :: Type

  -- | Annotate value
  annotate :: a -> Annotated a

  -- | Drop annotation
  --
  -- NOTE: 'Annotated' is a non-injective type family. You might need to supply
  -- a type argument to 'dropAnnotation' if the resulting type is not clear
  -- from context.
  dropAnnotation :: Annotated a -> a

{-------------------------------------------------------------------------------
  Deriving via support: computing annotations
-------------------------------------------------------------------------------}

-- | Annotation of a value
--
-- Unlike 'Annotated', 'Annotation' is not always defined: not all types /have/
-- annotations (for example, @Annotated a@ might just be @a@).
type family Annotation a :: Type

-- | Deriving via support: computing annotations
--
-- If you need to compute an annotation and do not need to worry about
-- annotating any nested values, you define a 'CanAnnotate' instance for some
-- type @A@ with annotation @B@ as follows:
--
-- > data A = ..
-- >   deriving CanAnnotate via PairWithAnnotation A
newtype PairWithAnnotation a = PairWithAnnotation a

class ComputeAnnotation a where
  computeAnnotation :: a -> Annotation a

instance ComputeAnnotation a => CanAnnotate (PairWithAnnotation a) where
  type Annotated (PairWithAnnotation a) = WithAnnotation a (Annotation a)
  annotate (PairWithAnnotation x) = WithAnnotation {
        value      = x
      , annotation = computeAnnotation x
      }
  dropAnnotation = PairWithAnnotation . value

data WithAnnotation a b = WithAnnotation {
      value      :: a
    , annotation :: b
    }
  deriving stock (Show, GHC.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
  deriving anyclass (Structured.Show)

{-------------------------------------------------------------------------------
  Deriving via: default instance for foldable containers
-------------------------------------------------------------------------------}

-- | Deriving via: default instance for foldable containers
--
-- We annotate the values in the containers, and give the container length as
-- its own annotation.
--
-- Example:
--
-- > deriving
-- >   via AnnotateFoldable [] a
-- >   instance CanAnnotate a => CanAnnotate [a]
newtype AnnotateFoldable f a = AnnotateFoldable (f a)

newtype Length = Length Int
  deriving stock (Show, GHC.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
  deriving anyclass (Structured.Show)

type instance Annotation (AnnotateFoldable f a) = Length

instance ( Functor f
         , Foldable f
         , CanAnnotate a
         ) => CanAnnotate (AnnotateFoldable f a) where
  type Annotated (AnnotateFoldable f a) = WithAnnotation (f (Annotated a)) Length
  annotate (AnnotateFoldable xs) = WithAnnotation {
        value      = annotate <$> xs
      , annotation = Length $ length xs
      }
  dropAnnotation = AnnotateFoldable . fmap dropAnnotation . value

{-------------------------------------------------------------------------------
  Deriving-via: no annotation
-------------------------------------------------------------------------------}

-- | Deriving via: no annotation
--
-- Example:
--
-- > data A = ..
-- >   deriving CanAnnotate via NoAnnotation A
newtype NoAnnotation a = NoAnnotation a

type instance Annotation (NoAnnotation a) = ()

instance CanAnnotate (NoAnnotation a) where
  type Annotated (NoAnnotation a) = a
  annotate (NoAnnotation x) = x
  dropAnnotation = NoAnnotation

{-------------------------------------------------------------------------------
  generics-sop auxiliary: reasoning about AllZip and AllZip2
-------------------------------------------------------------------------------}

class    c y x => Inv (c :: l -> k -> Constraint) (x :: k) (y :: l)
instance c y x => Inv (c :: l -> k -> Constraint) (x :: k) (y :: l)

data Dict2 (c :: k -> k -> Constraint) (x :: k) (y :: k) where
  Dict2 :: c x y => Dict2 c x y

invZip :: forall k c (xs :: [k]) (ys :: [k]).
     SOP.AllZip c xs ys
  => Proxy c
  -> Proxy ys
  -> Proxy xs
  -> Dict2 (SOP.AllZip (Inv c)) ys xs
invZip _ _ _ = go SOP.shape SOP.shape
  where
    go :: forall xs' ys'.
         SOP.AllZip c xs' ys'
      => SOP.Shape xs' -> SOP.Shape ys' -> Dict2 (SOP.AllZip (Inv c)) ys' xs'
    go SOP.ShapeNil        SOP.ShapeNil      = Dict2
    go (SOP.ShapeCons xs) (SOP.ShapeCons ys) =
        case go xs ys of
          Dict2 -> Dict2

zipImplies :: forall k c d (xs :: [k])  (ys :: [k]).
      SOP.AllZip c xs ys
   => Proxy c
   -> Proxy d
   -> Proxy xs
   -> Proxy ys
   -> (forall x y. c x y => Proxy x -> Proxy y -> Dict2 d x y)
   -> Dict2 (SOP.AllZip d) xs ys
zipImplies _ _ _ _ f = go SOP.shape SOP.shape
  where
    go :: forall xs' ys'.
         SOP.AllZip c xs' ys'
      => SOP.Shape xs' -> SOP.Shape ys' -> Dict2 (SOP.AllZip d) xs' ys'
    go SOP.ShapeNil        SOP.ShapeNil      = Dict2
    go (SOP.ShapeCons xs) (SOP.ShapeCons ys) =
        case (f (Proxy @(SOP.Head xs')) (Proxy @(SOP.Head ys')), go xs ys) of
          (Dict2, Dict2) -> Dict2

invAllZip :: forall k (c :: k -> k -> Constraint) (xss :: [[k]]) (yss :: [[k]]).
     SOP.AllZip (Inv (SOP.AllZip c)) xss yss
  => Proxy c
  -> Proxy xss
  -> Proxy yss
  -> Dict2 (SOP.AllZip (SOP.AllZip (Inv c))) xss yss
invAllZip pc pxss pyss =
    zipImplies
      (Proxy @(Inv (SOP.AllZip c)))
      (Proxy @(SOP.AllZip (Inv c)))
      pxss
      pyss
      (invZip pc)

invZip2 :: forall k c (xss :: [[k]]) (yss :: [[k]]).
     SOP.AllZip2 c xss yss
  => Proxy c
  -> Proxy yss
  -> Proxy xss
  -> Dict2 (SOP.AllZip2 (Inv c)) yss xss
invZip2 pc pyss pxss =
    case invZip (Proxy @(SOP.AllZip c)) pyss pxss of
      Dict2 -> case invAllZip pc pyss pxss of
                 Dict2 -> Dict2

htransInv_SOP :: forall k c f g (xss :: [[k]]) (yss :: [[k]]).
     SOP.AllZip2 c yss xss
  => Proxy c
  -> (forall x y. c y x => f x -> g y)
  -> SOP f xss
  -> SOP g yss
htransInv_SOP pc f =
    case invZip2 pc (Proxy @xss) (Proxy @yss) of
      Dict2 -> SOP.htrans (Proxy @(Inv c)) f

{-------------------------------------------------------------------------------
  Internal auxiliary: two-parameter wrapper around 'CanAnnotate'

  We use this for a generic @htrans@.
-------------------------------------------------------------------------------}

class Annotate' a b where
  annotate'       :: a -> b
  dropAnnotation' :: b -> a

instance (CanAnnotate a, b ~ Annotated a) => Annotate' a b where
  annotate'       = annotate
  dropAnnotation' = dropAnnotation

{-------------------------------------------------------------------------------
  Deriving via: generics
-------------------------------------------------------------------------------}

-- | Deriving via: annotate generically
--
-- This combinator can be used to define 'CanAnnotate' instance that just
-- walk over the structure of the argument, without adding any annotations
-- of their own.
--
-- Example:
--
-- > deriving
-- >   via AnnotateGenericallyAs (Maybe (Annotated a)) (Maybe a)
-- >   instance CanAnnotate a => CanAnnotate (Maybe a)
newtype AnnotateGenericallyAs b a = AnnotateGenericallyAs a

type instance Annotation (AnnotateGenericallyAs b a) = ()

instance ( SOP.Generic a
         , SOP.Generic b
         , SOP.AllZip2 Annotate' (SOP.Code a) (SOP.Code b)
         ) => CanAnnotate (AnnotateGenericallyAs b a) where

  type Annotated (AnnotateGenericallyAs b a) = b

  annotate (AnnotateGenericallyAs x) =
        SOP.to
      . SOP.htrans (Proxy @Annotate') (SOP.mapII annotate')
      . SOP.from
      $ x

  dropAnnotation =
        AnnotateGenericallyAs
      . SOP.to
      . htransInv_SOP (Proxy @Annotate') (SOP.mapII dropAnnotation')
      . SOP.from

{-------------------------------------------------------------------------------
  Standard instances: no annotation
-------------------------------------------------------------------------------}

deriving via NoAnnotation Bool        instance CanAnnotate Bool
deriving via NoAnnotation Aeson.Value instance CanAnnotate Aeson.Value

deriving via NoAnnotation Integer instance CanAnnotate Integer

deriving via NoAnnotation Int     instance CanAnnotate Int
deriving via NoAnnotation Int8    instance CanAnnotate Int8
deriving via NoAnnotation Int16   instance CanAnnotate Int16
deriving via NoAnnotation Int32   instance CanAnnotate Int32
deriving via NoAnnotation Int64   instance CanAnnotate Int64
deriving via NoAnnotation Int128  instance CanAnnotate Int128

deriving via NoAnnotation Word    instance CanAnnotate Word
deriving via NoAnnotation Word8   instance CanAnnotate Word8
deriving via NoAnnotation Word16  instance CanAnnotate Word16
deriving via NoAnnotation Word32  instance CanAnnotate Word32
deriving via NoAnnotation Word64  instance CanAnnotate Word64
deriving via NoAnnotation Word128 instance CanAnnotate Word128

deriving via NoAnnotation Float   instance CanAnnotate Float
deriving via NoAnnotation Double  instance CanAnnotate Double

{-------------------------------------------------------------------------------
  Standard instances: foldable
-------------------------------------------------------------------------------}

deriving
  via AnnotateFoldable [] a
  instance CanAnnotate a => CanAnnotate [a]

deriving
  via AnnotateFoldable (Map k) a
  instance CanAnnotate a => CanAnnotate (Map k a)

{-------------------------------------------------------------------------------
  Standard instances: generic
-------------------------------------------------------------------------------}

deriving
  via AnnotateGenericallyAs (Maybe (Annotated a)) (Maybe a)
  instance CanAnnotate a => CanAnnotate (Maybe a)

deriving
  via AnnotateGenericallyAs (Either (Annotated a) (Annotated b)) (Either a b)
  instance (CanAnnotate a, CanAnnotate b) => CanAnnotate (Either a b)

deriving
  via AnnotateGenericallyAs (Identity (Annotated a)) (Identity a)
  instance CanAnnotate a => CanAnnotate (Identity a)

{-------------------------------------------------------------------------------
  Standard instances: tuples

  These instances also use 'AnnotateGenericallyAs'.
-------------------------------------------------------------------------------}

-- 0
deriving
  via NoAnnotation ()
  instance CanAnnotate ()

-- 1 ('Solo' does not support SOP generics)
instance CanAnnotate a => CanAnnotate (Solo a) where
  type Annotated (Solo a) = Solo (Annotated a)
  annotate (Solo x) = Solo (annotate x)
  dropAnnotation (Solo x) = Solo (dropAnnotation x)

-- 2
deriving
  via AnnotateGenericallyAs
        ( Annotated a
        , Annotated b
        )
        (a, b)
  instance ( CanAnnotate a
           , CanAnnotate b
           ) => CanAnnotate (a, b)

-- 3
deriving
  via AnnotateGenericallyAs
        ( Annotated a
        , Annotated b
        , Annotated c
        )
        (a, b, c)
  instance ( CanAnnotate a
           , CanAnnotate b
           , CanAnnotate c
           ) => CanAnnotate (a, b, c)

-- 4
deriving
  via AnnotateGenericallyAs
        ( Annotated a
        , Annotated b
        , Annotated c
        , Annotated d
        )
        (a, b, c, d)
  instance ( CanAnnotate a
           , CanAnnotate b
           , CanAnnotate c
           , CanAnnotate d
           ) => CanAnnotate (a, b, c, d)

-- 5
deriving
  via AnnotateGenericallyAs
        ( Annotated a
        , Annotated b
        , Annotated c
        , Annotated d
        , Annotated e
        )
        (a, b, c, d, e)
  instance ( CanAnnotate a
           , CanAnnotate b
           , CanAnnotate c
           , CanAnnotate d
           , CanAnnotate e
           ) => CanAnnotate (a, b, c, d, e)

-- 6
deriving
  via AnnotateGenericallyAs
        ( Annotated a
        , Annotated b
        , Annotated c
        , Annotated d
        , Annotated e
        , Annotated f
        )
        (a, b, c, d, e, f)
  instance ( CanAnnotate a
           , CanAnnotate b
           , CanAnnotate c
           , CanAnnotate d
           , CanAnnotate e
           , CanAnnotate f
           ) => CanAnnotate (a, b, c, d, e, f)

-- 7
deriving
  via AnnotateGenericallyAs
        ( Annotated a
        , Annotated b
        , Annotated c
        , Annotated d
        , Annotated e
        , Annotated f
        , Annotated g
        )
        (a, b, c, d, e, f, g)
  instance ( CanAnnotate a
           , CanAnnotate b
           , CanAnnotate c
           , CanAnnotate d
           , CanAnnotate e
           , CanAnnotate f
           , CanAnnotate g
           ) => CanAnnotate (a, b, c, d, e, f, g)

-- 8
deriving
  via AnnotateGenericallyAs
        ( Annotated a
        , Annotated b
        , Annotated c
        , Annotated d
        , Annotated e
        , Annotated f
        , Annotated g
        , Annotated h
        )
        (a, b, c, d, e, f, g, h)
  instance ( CanAnnotate a
           , CanAnnotate b
           , CanAnnotate c
           , CanAnnotate d
           , CanAnnotate e
           , CanAnnotate f
           , CanAnnotate g
           , CanAnnotate h
           ) => CanAnnotate (a, b, c, d, e, f, g, h)

-- 9
deriving
  via AnnotateGenericallyAs
        ( Annotated a
        , Annotated b
        , Annotated c
        , Annotated d
        , Annotated e
        , Annotated f
        , Annotated g
        , Annotated h
        , Annotated i
        )
        (a, b, c, d, e, f, g, h, i)
  instance ( CanAnnotate a
           , CanAnnotate b
           , CanAnnotate c
           , CanAnnotate d
           , CanAnnotate e
           , CanAnnotate f
           , CanAnnotate g
           , CanAnnotate h
           , CanAnnotate i
           ) => CanAnnotate (a, b, c, d, e, f, g, h, i)
