{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Pretty-print value in a way that is valid Haskell
--
-- Intended for qualified import
--
-- > import qualified Data.Structured as Structured
module Data.Structured (
    Show(..)
  , show
  , showsPrec
  , print
    -- * Structured values
  , Value(..)
    -- * Generics
  , gtoValue
  , gtoValueAfter
  , sopToValue
  , sopToValueAfter
    -- * Deriving-via support
  , ToPreludeShow(..)
  , FromPreludeShow(..)
  ) where

import Prelude hiding (Show(..), print)
import qualified Prelude

import Control.Monad
import Data.Bifunctor
import Data.ByteString.Short (ShortByteString)
import Data.Default
import Data.Functor.Identity
import Data.Int
import Data.List (intersperse)
import Data.List.NonEmpty (NonEmpty(..))
import Data.SOP
import Data.SOP.Dict
import Data.String
import Data.Text (Text)
import Data.Text.Lazy.Builder (Builder)
import Data.Tuple.Solo
import Data.Typeable
import Data.WideWord
import Data.Word
import GHC.Show (appPrec)

import qualified Data.Aeson             as Aeson
import qualified Data.Aeson.KeyMap      as Aeson.KeyMap
import qualified Data.Aeson.Text        as Aeson
import qualified Data.ByteString        as Strict
import qualified Data.ByteString.Lazy   as Lazy
import qualified Data.List.NonEmpty     as NE
import qualified Data.Text              as Text
import qualified Data.Text.Lazy         as Lazy
import qualified Data.Text.Lazy         as Text.Lazy
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Vector            as Vector
import qualified Generics.SOP           as SOP

{-------------------------------------------------------------------------------
  Main API
-------------------------------------------------------------------------------}

-- | Pretty-print value in a way that is valid Haskell
--
-- This is similar to what @pretty-show@ offers, but @pretty-show@ does not
-- guarantee valid Haskell (for example, strings are shown without quotes).
class Show a where
  -- | Generate structured value
  --
  -- Typically instances are derived using generics:
  --
  -- > data MyType = ..
  -- >   deriving stock (GHC.Generic)
  -- >   deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
  -- >   deriving anyclass (Structured.Show)
  --
  -- If you want to tweak the generic instance, see 'gtoValueAfter'.
  toValue :: a -> Value

  default toValue ::
       (SOP.HasDatatypeInfo a, All2 Show (SOP.Code a))
    => a -> Value
  toValue = gtoValue

show :: Show a => a -> String
show = render False . toValue

showsPrec :: Show a => Int -> a -> ShowS
showsPrec p = showString . render (p > appPrec) . toValue

print :: Show a => a -> IO ()
print = putStrLn . show

{-------------------------------------------------------------------------------
  Value
-------------------------------------------------------------------------------}

-- | Structured value
data Value where
  -- | Constructor (or smart constructor) application
  --
  -- We allow for some type applications, too.
  Constr :: Text -> [TypeRep] -> [Value] -> Value

  -- | Record
  Record :: Text -> [(String, Value)] -> Value

  -- | List
  List :: [Value] -> Value

  -- | Tuple
  Tuple :: [Value] -> Value

  -- | Anything String-like
  --
  -- Precondition: the 'Show' and 'IsString' instances must be compatible.
  String :: forall a. (Prelude.Show a, IsString a) => a -> Value

  -- | Integral numbers
  --
  -- These are shown assuming @NumericUnderscores@.
  Integral :: forall a. (Prelude.Show a, Integral a) => a -> Value

  -- | Quasi-quote
  --
  -- We separate out the quasi-quoter from the quoted string proper.
  -- The lines of the quasi-quoted string are listed separately.
  QuasiQ :: Text -> NonEmpty Builder -> Value

  -- | JSON value
  --
  -- We define this as an explicit constructor, in case we ever want to
  -- generate structured JSON logs from these values.
  --
  -- The pretty-printer uses the @aesonQQ@ quasi-quoter.
  JSON :: Aeson.Value -> Value

  -- | Value shown using the Prelude's show
  PreludeShow :: Prelude.Show a => a -> Value

deriving instance Prelude.Show Value

{-------------------------------------------------------------------------------
  Deriving-via support
-------------------------------------------------------------------------------}

-- | Derive 'Prelude.Show' through 'Show'
--
-- You might not want to do always do this; in some circumstances it may be
-- useful to have a non-pretty-printed 'Show' instance alongside 'Show'.
newtype ToPreludeShow a = ToPreludeShow a

-- | Derive 'Show' through 'Prelude.Show'
--
-- NOTE: This should be used sparingly. When 'Show x' is derived using
-- 'Prelude.Show x', the result should still be a law-abiding instance (generate
-- valid Haskell code), assuming that the 'Prelude.Show' instance is
-- law-abiding; however, it will limit the ability to generate structured
-- values in different formats, such as JSON.
newtype FromPreludeShow a = FromPreludeShow a

instance Show a => Prelude.Show (ToPreludeShow a) where
  showsPrec p (ToPreludeShow x) = showsPrec p x

instance Prelude.Show a => Show (FromPreludeShow a) where
  toValue (FromPreludeShow x) = PreludeShow x

{-------------------------------------------------------------------------------
  Generics
-------------------------------------------------------------------------------}

-- | Newtype which is transparent for the purposes of 'Show'
--
-- This is only used internally in 'sopToValue'.
newtype Transparent a = Transparent a
  deriving newtype Show

gtoValue :: forall a.
     (SOP.HasDatatypeInfo a, All2 Show (SOP.Code a))
  => a -> Value
gtoValue x = sopToValue (SOP.from x) (SOP.datatypeInfo (Proxy @a))

-- | Generic derivation of 'toValue'
--
-- The standard generics instance will depend on 'Show' for all nested values.
-- This is usually the right choice, but occassionally you will want to show
-- nested values in a different manner; in this case, you can use
-- 'gtoValueAfter'. Example:
--
-- > instance Structured.Show RecordB where
-- >   toValue = Structured.gtoValueAfter Structured.FromPreludeShow
--
-- (However, 'FromPreludeShow' should be used sparingly; see discussion there.)
gtoValueAfter :: forall f a.
     (SOP.HasDatatypeInfo a, All2 (Compose Show f) (SOP.Code a))
  => (forall x. x -> f x)
  -> a -> Value
gtoValueAfter f x = sopToValueAfter f (SOP.from x) (SOP.datatypeInfo (Proxy @a))

sopToValue :: forall xss.
     All2 Show xss
  => SOP I xss -> SOP.DatatypeInfo xss -> Value
sopToValue =
    case aux of Dict -> sopToValueAfter Transparent
  where
    aux :: Dict (All2 (Compose Show Transparent)) xss
    aux = all_POP $ hcpure (Proxy @Show) Dict

sopToValueAfter :: forall f xss.
     All2 (Compose Show f) xss
  => (forall x. x -> f x)
  -> SOP I xss -> SOP.DatatypeInfo xss -> Value
sopToValueAfter f (SOP xss) info = hcollapse $
    hczipWith (Proxy @(All (Compose Show f))) aux (SOP.constructorInfo info) xss
  where
    aux ::
         All (Compose Show f) xs
      => SOP.ConstructorInfo xs -> NP I xs -> K Value xs
    aux (SOP.Constructor name)   xs = K $ auxSimple name xs
    aux (SOP.Record name fields) xs = K $ auxRecord name fields xs
    aux (SOP.Infix _ _ _)        _  = error "sopToValue: TODO: infix"

    auxSimple ::
         All (Compose Show f) xs
      => String -> NP I xs -> Value
    auxSimple constr xs = Constr (Text.pack constr) [] $ hcollapse $
        hcmap (Proxy @(Compose Show f)) (mapIK (toValue . f)) xs

    auxRecord ::
         All (Compose Show f) xs
      => SOP.ConstructorName -> NP SOP.FieldInfo xs -> NP I xs -> Value
    auxRecord constr fields xs = Record (Text.pack constr) $ hcollapse $
        hczipWith (Proxy @(Compose Show f)) auxRecordField fields xs

    auxRecordField ::
         Show (f x)
      => SOP.FieldInfo x -> I x -> K (String, Value) x
    auxRecordField field (I x) = K (SOP.fieldName field, toValue (f x))

{-------------------------------------------------------------------------------
  Standard instances
-------------------------------------------------------------------------------}

instance Show Word    where toValue = Integral
instance Show Word8   where toValue = Integral
instance Show Word16  where toValue = Integral
instance Show Word32  where toValue = Integral
instance Show Word64  where toValue = Integral
instance Show Word128 where toValue = Integral

instance Show Int     where toValue = Integral
instance Show Int8    where toValue = Integral
instance Show Int16   where toValue = Integral
instance Show Int32   where toValue = Integral
instance Show Int64   where toValue = Integral
instance Show Int128  where toValue = Integral

instance Show Integer where toValue = Integral

instance {-# OVERLAPPABLE #-} Show a => Show [a] where
  toValue = List . map toValue

instance Show Aeson.Value where
  toValue = JSON

instance Typeable a => Show (Proxy (a :: k)) where
  toValue p = Constr "Proxy" [typeRep p] []

{-------------------------------------------------------------------------------
  String-like types
-------------------------------------------------------------------------------}

instance {-# OVERLAPPING #-} Show String where
  toValue = String

instance Show Strict.ByteString where toValue = String
instance Show Lazy.ByteString   where toValue = String
instance Show ShortByteString   where toValue = String
instance Show Text              where toValue = String
instance Show Lazy.Text         where toValue = String

{-------------------------------------------------------------------------------
  Tuples
-------------------------------------------------------------------------------}

-- 0
instance Show ()

-- 1 (Solo does not support SOP generics)
instance Show a => Show (Solo a) where
  toValue (Solo x) = Constr "Solo" [] [toValue x]

-- 2
instance ( Show a
         , Show b
         ) => Show (a, b) where
  toValue (a, b) = Tuple [
        toValue a
      , toValue b
      ]

-- 3
instance ( Show a
         , Show b
         , Show c
         ) => Show (a, b, c) where
  toValue (a, b, c) = Tuple [
        toValue a
      , toValue b
      , toValue c
      ]

-- 4
instance ( Show a
         , Show b
         , Show c
         , Show d
         ) => Show (a, b, c, d) where
  toValue (a, b, c, d) = Tuple [
        toValue a
      , toValue b
      , toValue c
      , toValue d
      ]

-- 5
instance ( Show a
         , Show b
         , Show c
         , Show d
         , Show e
         ) => Show (a, b, c, d, e) where
  toValue (a, b, c, d, e) = Tuple [
        toValue a
      , toValue b
      , toValue c
      , toValue d
      , toValue e
      ]

-- 6
instance ( Show a
         , Show b
         , Show c
         , Show d
         , Show e
         , Show f
         ) => Show (a, b, c, d, e, f) where
  toValue (a, b, c, d, e, f) = Tuple [
        toValue a
      , toValue b
      , toValue c
      , toValue d
      , toValue e
      , toValue f
      ]

-- 7
instance ( Show a
         , Show b
         , Show c
         , Show d
         , Show e
         , Show f
         , Show g
         ) => Show (a, b, c, d, e, f, g) where
  toValue (a, b, c, d, e, f, g) = Tuple [
        toValue a
      , toValue b
      , toValue c
      , toValue d
      , toValue e
      , toValue f
      , toValue g
      ]

-- 8
instance ( Show a
         , Show b
         , Show c
         , Show d
         , Show e
         , Show f
         , Show g
         , Show h
         ) => Show (a, b, c, d, e, f, g, h) where
  toValue (a, b, c, d, e, f, g, h) = Tuple [
        toValue a
      , toValue b
      , toValue c
      , toValue d
      , toValue e
      , toValue f
      , toValue g
      , toValue h
      ]

-- 9
instance ( Show a
         , Show b
         , Show c
         , Show d
         , Show e
         , Show f
         , Show g
         , Show h
         , Show i
         ) => Show (a, b, c, d, e, f, g, h, i) where
  toValue (a, b, c, d, e, f, g, h, i) = Tuple [
        toValue a
      , toValue b
      , toValue c
      , toValue d
      , toValue e
      , toValue f
      , toValue g
      , toValue h
      , toValue i
      ]

{-------------------------------------------------------------------------------
  Instances that rely on generics
-------------------------------------------------------------------------------}

instance Show Bool

instance Show a => Show (Maybe a)

instance (Show a, Show b) => Show (Either a b)

instance Show a => Show (Identity a)

{-------------------------------------------------------------------------------
  Rendering proper
-------------------------------------------------------------------------------}

render ::
     Bool -- ^ Are we in a context that may require brackets?
  -> Value -> String
render = \contextNeedsBrackets ->
      Text.Lazy.unpack
    . B.toLazyText
    . intercalate "\n"
    . NE.toList
    . go contextNeedsBrackets
  where
    go :: Bool -> Value -> NonEmpty Builder
    go contextneedsBrackets val =
        bracketIf (contextneedsBrackets && requiresBrackets val) $
          case val of
            Integral x     -> simple $ addNumericUnderscores (Prelude.show x)
            String x       -> simple $ Prelude.show x
            Constr c ts xs -> renderComposite (compositeConstr c ts) $
                                map (go True) xs
            List xs        -> renderComposite compositeList $
                                map (go False) xs
            Tuple xs       -> renderComposite compositeTuple $
                                map (go False) xs
            Record r xs    -> renderComposite (compositeHaskellRecord r) $
                                map (uncurry goField . second (go False)) xs
            QuasiQ qq str  -> renderComposite (compositeQuasiQ qq) [str]
            JSON json      -> go contextneedsBrackets $ QuasiQ "aesonQQ" $
                                renderJSON json
            PreludeShow x  -> NE.fromList . map B.fromString $
                                lines (Prelude.showsPrec appPrec x "")

    simple :: String -> NonEmpty Builder
    simple = pure . B.fromString

    goField :: String -> NonEmpty Builder -> NonEmpty Builder
    goField field (firstLine :| rest) =
           (B.fromString field <> " = " <> firstLine)
        :| indent rest

    bracketIf :: Bool -> NonEmpty Builder -> NonEmpty Builder
    bracketIf False = id
    bracketIf True  = \case
        oneLine   :| []   -> ("(" <> oneLine <> ")") :| []
        firstLine :| rest -> ("( " <> firstLine) :| concat [
              indent rest
            , [")"]
            ]

renderJSON :: Aeson.Value -> NonEmpty Builder
renderJSON = go
  where
    go :: Aeson.Value -> NonEmpty Builder
    go (Aeson.Object xs) = renderComposite compositeJsonRecord $
                             map (uncurry goField . second go) $
                               Aeson.KeyMap.toList xs
    go (Aeson.Array xs)  = renderComposite compositeList $
                             map go $
                               Vector.toList xs
    go val               = Aeson.encodeToTextBuilder val :| []

    goField :: Aeson.KeyMap.Key -> NonEmpty Builder -> NonEmpty Builder
    goField key (firstLine :| rest) =
           B.fromString (Prelude.show key) <> ": " <> firstLine
        :| map ("  " <>) rest

-- | Does this value require brackets when shown?
--
-- Of course, these brackets will only be necessary if the context demands them.
requiresBrackets :: Value -> Bool
requiresBrackets = \case
    Constr _ ts xs -> not (null ts) || not (null xs)
    _otherwise     -> False

{-------------------------------------------------------------------------------
  Internal: rendering composite values
-------------------------------------------------------------------------------}

data Composite = Composite {
      -- | Header (e.g. record name, type applications, ..)
      compositeHeader :: Maybe Text

      -- | Prefix (e.g. @{@ or @(@)
    , compositePrefix :: Maybe Text

      -- | Suffix (e.g, @}@ or @)@)
    , compositeSuffix :: Maybe Text

      -- | Element separator (e.g. @,@)
    , compositeSeparator :: Char

      -- | Should elements be shown on one line?
      --
      -- By default, this is true only if there is only a single element,
      -- and that element is itself only one line.
    , compositeOneLine :: [NonEmpty Builder] -> Maybe [Builder]
    }

instance Default Composite where
  def = Composite {
      compositeHeader    = Nothing
    , compositePrefix    = Nothing
    , compositeSuffix    = Nothing
    , compositeSeparator = ','
    , compositeOneLine   = \case
                             [firstLine :| []] -> Just [firstLine]
                             _otherwise        -> Nothing
    }

compositeList :: Composite
compositeList = def {
      compositePrefix  = Just "["
    , compositeSuffix  = Just "]"
    , compositeOneLine = \rs -> do
        xs <- mapM isOneLine rs

        let argLengths :: [Int64]
            argLengths = map (Text.Lazy.length . B.toLazyText) xs
        guard $ or [
            sum argLengths < 80
          , all (<= 5) argLengths
          ]

        return xs
    }
  where
    isOneLine :: NonEmpty a -> Maybe a
    isOneLine (firstLine :| []) = Just firstLine
    isOneLine _otherwise        = Nothing

compositeTuple :: Composite
compositeTuple = def {
      compositePrefix = Just "("
    , compositeSuffix = Just ")"
    }

compositeConstr :: Text -> [TypeRep] -> Composite
compositeConstr c ts = def {
      compositeSeparator = ' '
    , compositeHeader    = Just $ intercalate " " $ c : map typeApp ts
    }
  where
    -- We are careful to insert brackets around the typerep if needed
    typeApp :: TypeRep -> Text
    typeApp typ = "@" <> Text.pack (Prelude.showsPrec appPrec typ [])

compositeHaskellRecord :: Text -> Composite
compositeHaskellRecord r = def {
      compositeHeader  = Just $ r
    , compositePrefix  = Just $ "{"
    , compositeSuffix  = Just $ "}"
    , compositeOneLine = const Nothing
    }

compositeJsonRecord :: Composite
compositeJsonRecord = def {
      compositePrefix = Just $ "{"
    , compositeSuffix = Just $ "}"
    , compositeOneLine = const Nothing
    }

compositeQuasiQ :: Text -> Composite
compositeQuasiQ qq = def {
      compositePrefix  = Just $ "[" <> qq <> "|"
    , compositeSuffix  = Just $ "|]"
    }

-- | Render composite value
renderComposite :: Composite -> [NonEmpty Builder] -> NonEmpty Builder
renderComposite Composite{..} =
    go
  where
    go :: [NonEmpty Builder] -> NonEmpty Builder
    go rs
      | Just xs <- compositeOneLine rs
      = pure $ mconcat [
           prefix
         , intercalate (B.singleton compositeSeparator) xs
         , maybe mempty B.fromText compositeSuffix
         ]

      | otherwise
      =    prefix
        :| concat [
               concatMap NE.toList $ sepElemsBy compositeSeparator rs
             , [B.fromText suffix | Just suffix <- [compositeSuffix]]
             ]

    prefix :: Builder
    prefix = mconcat [
                 maybe mempty (\hdr -> B.fromText hdr <> " ") compositeHeader
               , maybe mempty B.fromText compositePrefix
               ]

    sepElemsBy :: Char -> [NonEmpty Builder] -> [NonEmpty Builder]
    sepElemsBy sep = zipWith aux (True : repeat False)
      where
        aux :: Bool -> NonEmpty Builder -> NonEmpty Builder
        aux firstEntry (firstLine :| rest) =
               ( if firstEntry
                   then (B.singleton ' ' <> B.singleton ' ' <> firstLine)
                   else (B.singleton sep <> B.singleton ' ' <> firstLine)
               )
            :| indent rest

{-------------------------------------------------------------------------------
  Rendering auxiliary
-------------------------------------------------------------------------------}

indent :: [Builder] -> [Builder]
indent = map (B.fromText "  " <>)

addNumericUnderscores :: String -> String
addNumericUnderscores =
      reverse
    . aux
    . reverse
  where
    aux :: String -> String
    aux str =
        case splitAt 3 str of
          (_          , []  ) -> str
          (firstThree , rest) -> firstThree ++ "_" ++ aux rest

-- | Generation of @intercalate@ from the Prelude
intercalate :: Monoid a => a -> [a] -> a
intercalate x = mconcat . intersperse x