{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
module Data.Text.Prettyprint.EDoc.Internal
( EDoc(..)
, Delim(..)
) where

import Language.Haskell.Meta.Parse ( parseExp )
import Data.Text.Prettyprint.Doc ( Pretty, Doc, pretty )
import qualified Data.Text.Prettyprint.Doc as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Text as Render
import Data.Generics
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote
import Text.Parsec
import qualified System.IO as IO

data Delim = Parens | Brackets | Braces | Angles
  deriving (Show, Data)

pprDelim :: Delim -> Doc ann -> Doc ann
pprDelim Parens   = Pretty.parens
pprDelim Brackets = Pretty.brackets
pprDelim Braces   = Pretty.braces
pprDelim Angles   = Pretty.angles

data EDoc = forall e. Pretty e => Hs e
         | Wrap Delim EDoc | HCat [EDoc] | HSep [EDoc]
         | VCat [EDoc] | VSep [EDoc] | Nest Int EDoc

instance Pretty EDoc where
  pretty (Hs     x) = pretty x
  pretty (Wrap p x) = pprDelim p $ pretty x
  pretty (HCat l)   = Pretty.hcat $ map pretty l
  pretty (HSep l)   = Pretty.hsep $ map pretty l
  pretty (VCat l)   = Pretty.vcat $ map pretty l
  pretty (VSep l)   = Pretty.vsep $ map pretty l
  pretty (Nest i l) = Pretty.nest i $ pretty l

