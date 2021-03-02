
{-# LANGUAGE TypeOperators, Rank2Types, PatternSignatures #-}
{-# OPTIONS_GHC -Wall #-}
 
-------------------------------------------------------------------------------
 
{- |
Module      :  Galculator.Evaluate
Description :  
Copyright   :  (c) Paulo Silva
License     :  LGPL
 
Maintainer  :  paufil@di.uminho.pt
Stability   :  experimental
Portability :  portable
 
-}

-------------------------------------------------------------------------------

module Galculator.Evaluate (
  evalCombinator,
  evalDerivation
 ) where

import Control.GalcError
import Control.Monad.Error
import Galculator.Engine.GcToLaw
import Galculator.Engine.LawToRule
import Galculator.Rule
import Galculator.State
import Language.Combinator.Syntax
import Language.Derivation.Syntax
import Language.Law.Syntax
import Language.R.Rewrite
import Language.R.Syntax
import qualified Language.Type.Syntax as T
import Prelude hiding (all)

-------------------------------------------------------------------------------

combBin :: Combinator -> (Rule -> Rule -> Rule) -> Combinator -> GalcStateT Rule
combBin c1 comb c2 = do
  c1' <- evalCombinator c1
  c2' <- evalCombinator c2
  let rc = comb c1' c2'
  return rc

-------------------------------------------------------------------------------

combUni :: (Rule -> Rule) -> Combinator -> GalcStateT Rule
combUni comb c = do
  c' <- evalCombinator c
  let rc = comb c' 
  return rc

-------------------------------------------------------------------------------

evalCombinator :: Combinator -> GalcStateT Rule
evalCombinator Nop             = return nop
evalCombinator Fail            = return failM
evalCombinator (Seq c1 c2)     = combBin c1 (>>>) c2 
evalCombinator (Choice c1 c2)  = combBin c1 (|||) c2
evalCombinator (LChoice c1 c2) = combBin c1 (|<|) c2
evalCombinator (Many c)        = combUni many c 
evalCombinator (Many1 c)       = combUni many1 c 
evalCombinator (Try c)         = combUni try c
evalCombinator (Once c)        = combUni once c
evalCombinator (Everywhere c)  = combUni everywhere c
evalCombinator (Everywhere' c) = combUni everywhere' c
evalCombinator (Innermost c)   = combUni innermost c
evalCombinator (All c)         = combUni all c
evalCombinator (One c)         = combUni one c
evalCombinator (Rule r) = 
  case r of
    Inv drv -> do 
      lw <- evalDerivation drv
      let (rl::Rule) = getRuleInv lw
      return rl
    _ -> do
      lw <- evalDerivation r
      let (rl::Rule) = getRule lw
      return rl

-------------------------------------------------------------------------------

deriveLaw :: String -> (forall t . T.Type t -> R t -> GalcStateT Law) -> GalcStateT Law
deriveLaw ref f = do
  gc <- maybe2error (ReferenceError ref) =<< getGC ref
  rType2Law gc f

-------------------------------------------------------------------------------

evalDerivation :: Derivation -> GalcStateT Law
evalDerivation (Inv _)          = throwError DerivationError
evalDerivation (Shunt ref)      = deriveLaw ref gcShunting
evalDerivation (DistrLow ref)   = deriveLaw ref gcDistributivityLower
evalDerivation (DistrUp ref)    = deriveLaw ref gcDistributivityUpper
evalDerivation (MonotLow ref)   = deriveLaw ref gcMonotonicityLower
evalDerivation (MonotUp ref)    = deriveLaw ref gcMonotonicityUpper
evalDerivation (TopPreserv ref) = deriveLaw ref gcPreservingTop
evalDerivation (BotPreserv ref) = deriveLaw ref gcPreservingBottom
evalDerivation (CancUp ref)     = deriveLaw ref gcCancellationUpper
evalDerivation (CancLow ref)    = deriveLaw ref gcCancellationLower
evalDerivation (Free _)         = undefined
evalDerivation (Apply ref)      = getLaw ref

-------------------------------------------------------------------------------

