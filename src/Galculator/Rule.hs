
{-# LANGUAGE GADTs, TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall #-}

-------------------------------------------------------------------------------

{- |
Module      :  Galculator.Rule
Description :  
Copyright   :  (c) Paulo Silva
License     :  LGPL

Maintainer  :  paufil@di.uminho.pt
Stability   :  experimental
Portability :  portable
<description of the module>

-}

-------------------------------------------------------------------------------

module Galculator.Rule (
  Rule,
  RewriteStep,
  rewrite,
  successEquiv,
  successImpl
 ) where

import Control.MonadOr
import Control.MonadPosition
import Control.Monad.RWS
import Data.Maybe
import Language.Law.Syntax
import Language.R.Rewrite
import Language.R.Syntax
import Language.Type.Syntax

-------------------------------------------------------------------------------

type Rule = GenericM Rewrite
type Hyp = [String]

type RewriteSt = Pos
type Rewrite = RWST Hyp [RewriteStep] RewriteSt []

instance MonadOr Rewrite where
  morelse (RWST f) (RWST g) = RWST $ \r s -> f r s `morelse` g r s

instance MonadPosition Rewrite where
  branchLeft = do
    st <- get
    put (0:st)
  branchRight = do
    st <- get
    put (0:st)
  getPosition = get

-------------------------------------------------------------------------------
type RewriteStep = (Law, Pos)
type Pos = [Int]

-------------------------------------------------------------------------------
initialState :: [Int]
initialState = []
-------------------------------------------------------------------------------

rewrite :: Rule -> Type a -> R a -> Maybe ((R a), [RewriteStep])
rewrite r t expr = listToMaybe $ evalRWST (r t expr) [] initialState

-------------------------------------------------------------------------------

successEquiv :: Meta -> Type a -> R a -> R a -> Rewrite (R a)
successEquiv m t r r' = do
  pos <- getPosition
  tell [(EQUIV m t r r',pos)]
  return r'

-------------------------------------------------------------------------------

successImpl :: Meta -> Type a -> R a -> R a -> Rewrite (R a)
successImpl m t r r' = do
  pos <- getPosition
  tell [(IMPL m t r r',pos)]
  return r'

-------------------------------------------------------------------------------

