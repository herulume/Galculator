
{-# OPTIONS_GHC -Wall #-}

-------------------------------------------------------------------------------

{- |
Module      :  Galculator.Engine.LawToRule
Description :  Engine that extracts rules from laws.
Copyright   :  (c) Paulo Silva
License     :  LGPL

Maintainer  :  paufil@di.uminho.pt
Stability   :  experimental
Portability :  portable

-}

-------------------------------------------------------------------------------

module Galculator.Engine.LawToRule (
  getRule,
  getRuleInv
 ) where

import Control.Monad
import Data.Existential
import Galculator.Rule
import Language.Law.Syntax
import Language.R.Constraint
import Language.R.Match
import Language.R.SafeCast
import Language.Type.Constraint
import Language.Type.Unification

-------------------------------------------------------------------------------

getRule :: Law -> Rule
getRule (EQUIV m t1 r1 r2) t2 r = do
  rcns <- rConstraint r1 r
  cns <- unify $ [t1 :=: t2] ++ rcns
  Hide t1' <- typeRewrite cns t1
  r1' <- rCast cns t1' r1
  r'  <- rCast cns t1' r
  r2' <- rCast cns t2 r2
  cnsu <- rMatch r1' r'
  let r2u = rSubst cnsu t2 r2'
  successEquiv m t2 r r2u
  
getRule (IMPL m t1 r1 r2) t2 r = do
  rcns <- rConstraint r1 r
  cns <- unify $ [t1 :=: t2] ++ rcns
  Hide t1' <- typeRewrite cns t1
  r1' <- rCast cns t1' r1
  r'  <- rCast cns t1' r
  r2' <- rCast cns t2 r2
  cnsu <- rMatch r1' r'
  let r2u = rSubst cnsu t2 r2'
  successImpl m t2 r r2u

-------------------------------------------------------------------------------

getRuleInv :: Law -> Rule
getRuleInv (EQUIV m t1 r1 r2) t2 r = do
  rcns <- rConstraint r2 r
  cns <- unify $ [t1 :=: t2] ++ rcns
  Hide t1' <- typeRewrite cns t1
  r2' <- rCast cns t1' r2
  r' <- rCast cns t1' r
  r1' <- rCast cns t2 r1
  cnsu <- rMatch r2' r'
  let r1u = rSubst cnsu t2 r1'
  successEquiv m t2 r r1u
getRuleInv (IMPL _ _ _ _) _ _ = mzero

-------------------------------------------------------------------------------

