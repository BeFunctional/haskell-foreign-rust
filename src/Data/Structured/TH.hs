{-# LANGUAGE TemplateHaskell #-}

module Data.Structured.TH (
    deriveInstance
  ) where

import qualified Data.Structured as Structured
import qualified Data.Text       as Text

import Language.Haskell.TH
import Language.Haskell.TH.Datatype

-- | Derive 'Show' instance
--
-- Normally TH is not required, and you can rely on generics instead. However,
-- in some cases TH is the only option; for example, this is the case when
-- deriving a 'Show' instance for a GADT.
--
-- Example usage:
--
-- > Structured.deriveInstance 'ConstrOfMyType [t|
-- >     forall a. Structured.Show a => Structured.Show (MyType a)
-- >   |]
--
-- All type variables must be explicitly quantified (use an empty forall if
-- there are none), and any required constraints must be explicitly listed. In
-- addition, one of the constructors of @MyType@ must be listed (this is used to
-- resolve the datatype, ensuring that it works with regular datatypes as well
-- as associated datatypes).
deriveInstance :: Name -> Q Type -> Q [Dec]
deriveInstance constr header = do
    info        <- reifyDatatype constr
    (ctxt, rhs) <- parseHeader =<< header
    (:[]) <$>
      instanceD
        (return ctxt)
        (return rhs)
        [ funD 'Structured.toValue $ map caseFor (datatypeCons info)
        ]

-- | Parse instance header
parseHeader :: Type -> Q (Cxt, Type)
parseHeader = \case
      ForallT _bndrs ctxt rhs@(AppT (ConT nameShow) _)
        | nameShow == ''Structured.Show
        -> return (ctxt, rhs)
      _otherwise ->
        fail $ "Invalid header"

-- | Case for one of the constructors
caseFor :: ConstructorInfo -> Q Clause
caseFor ConstructorInfo{
            constructorName    = con
          , constructorFields  = fields
          , constructorVariant = variant
          } = do
    args <- mapM (const $ newName "x") fields
    clause
         [conP con (map varP args)]
         ( normalB $
             case variant of
               RecordConstructor fieldNames -> record fieldNames args
               _otherwise                   -> constr            args
         )
         []
  where
    -- Regular (non-record) constructor
    constr :: [Name] -> ExpQ
    constr args = appsE [
          conE 'Structured.Constr
        ,   varE 'Text.pack
          `appE`
            litE (StringL (nameBase con))
        , listE [] -- We do not support any type applications
        , listE $
            map
              (\x -> varE 'Structured.toValue `appE` varE x)
              args
        ]

    record :: [Name] -> [Name] -> ExpQ
    record fieldNames args = appsE [
          conE 'Structured.Record
        ,   varE 'Text.pack
          `appE`
            litE (StringL (nameBase con))
        , listE $
            zipWith
              (\f x -> tupE [
                  litE (StringL (nameBase f))
                , varE 'Structured.toValue `appE` varE x
                ]
              )
              fieldNames
              args
        ]
