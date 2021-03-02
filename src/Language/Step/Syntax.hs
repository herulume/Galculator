
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall #-}

-------------------------------------------------------------------------------

{- |
Module      :  Language.Step.Syntax
Description :  Abstract representation of proof steps.
Copyright   :  (c) Paulo Silva
License     :  LGPL
  
Maintainer  :  paufil@di.uminho.pt
Stability   :  experimental
Portability :  portable
  
-}
  
-------------------------------------------------------------------------------

module Language.Step.Syntax (
  Step(..),
  steps
 ) where

import Language.Combinator.Syntax
import Language.R.SyntaxADT

-------------------------------------------------------------------------------
  
data Step where
  Comb        :: Combinator -> Step
  Indirect    :: Either S S -> Step
  IndirectEnd :: Step
  LeftP       :: Step
  Qed         :: Step
  RightP      :: Step
  SeqC        :: Step -> Step -> Step
  deriving Show
  
-------------------------------------------------------------------------------

steps :: [String]
steps = ["indirect", "low", "up", "end", "left", "right", "seqc" ]