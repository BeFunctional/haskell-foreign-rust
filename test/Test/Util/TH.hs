{-# LANGUAGE TemplateHaskell #-}

module Test.Util.TH (
    reparseShow
  , reparseStructured
  ) where

import Data.String (fromString)

import qualified Data.Structured       as Structured
import qualified Language.Haskell.TH   as TH
import qualified Language.Haskell.Exts as Exts
import Language.Haskell.TH (Q)
import Language.Haskell.TH.Quote
import Data.Aeson.QQ.Simple

{-------------------------------------------------------------------------------
  Parse expressions
-------------------------------------------------------------------------------}

parseExp :: String -> Q TH.Exp
parseExp =
      toExp
    . Exts.fromParseResult
    . Exts.parseExpWithMode parseMode
  where
    parseMode :: Exts.ParseMode
    parseMode = Exts.defaultParseMode {
          Exts.extensions = [
              Exts.EnableExtension Exts.OverloadedStrings
            , Exts.EnableExtension Exts.TypeApplications
            , Exts.EnableExtension Exts.QuasiQuotes
            ]
        }

reparseShow :: Show a => a -> Q TH.Exp
reparseShow = parseExp . show

reparseStructured :: Structured.Show a => a -> Q TH.Exp
reparseStructured = parseExp . Structured.show

{-------------------------------------------------------------------------------
  Translate haskell-src-exts @Exp@ to TH @Exp@

  There is a package for this (@haskell-src-meta@), but it does not support
  overloaded string nor quasi-quotes, which makes it rather useless for our
  purposes. We only need to support a tiny handful of expressions, so we just
  define it ourselves.
-------------------------------------------------------------------------------}

toExp :: Exts.Exp Exts.SrcSpanInfo -> Q TH.Exp
toExp = \case

    -- Standard instances
    -- (These would presumably be similar in haskell-src-meta)

    Exts.Var _ (Exts.UnQual _ (Exts.Ident _ n)) ->
      pure $ TH.VarE $ TH.mkName n
    Exts.App _ e (Exts.TypeApp _ (Exts.TyCon _ (Exts.UnQual _ (Exts.Ident _ n)))) ->
      TH.AppTypeE <$> toExp e <*> pure (TH.ConT (TH.mkName n))
    Exts.App _ e1 e2 ->
      TH.AppE <$> toExp e1 <*> toExp e2
    Exts.List _ es ->
      TH.ListE <$> mapM toExp es
    Exts.Lit _ (Exts.Int _ x _) ->
      pure $ TH.LitE (TH.IntegerL x)

    -- Overloaded strings

    Exts.Lit _ (Exts.String _ x _) ->
      pure $ TH.AppE (TH.VarE 'fromString) (TH.LitE (TH.StringL x))

    -- Quasi-quotes

    Exts.QuasiQuote _ "aesonQQ" str ->
      quoteExp aesonQQ str

    -- Anything else is urecognized

    e -> fail $ "toExp: unrecognized expression " ++ show e

