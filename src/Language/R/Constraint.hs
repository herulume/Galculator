
{-# OPTIONS_GHC -Wall #-}

-------------------------------------------------------------------------------

{- |
Module      :  Language.R.Constraint
Description :  Type equation constraints infered from type annotations of 
               representations of functions.
Copyright   :  (c) Paulo Silva
License     :  LGPL

Maintainer  :  paufil@di.uminho.pt
Stability   :  experimental
Portability :  portable

-}

-------------------------------------------------------------------------------

module Language.R.Constraint (
  rConstraint
 ) where

import Control.Monad
import Control.MonadOr
import Language.R.Equality
import Language.R.Syntax
import Language.Type.Constraint

-------------------------------------------------------------------------------

rConstraint :: MonadOr m => R a -> R b -> m [Constraint]
rConstraint (Var _) _ = return []
rConstraint _ (Var _) = return []
rConstraint (NEG r) (NEG r') = do
  rr <- rConstraint r r'
  return rr
rConstraint (MEET r s) (MEET r' s') = do
  rr <- rConstraint r r'
  ss <- rConstraint s s'
  return $ rr ++ ss
rConstraint (JOIN r s) (JOIN r' s') = do
  rr <- rConstraint r r'
  ss <- rConstraint s s'
  return $ rr ++ ss
rConstraint (CONV r) (CONV r') = do
  rr <- rConstraint r r'
  return rr
rConstraint (COMP t f g) (COMP t' f' g') = do
  ff <- rConstraint f f'
  gg <- rConstraint g g'
  return $ [t :=: t'] ++ ff ++ gg
rConstraint (SPLIT r s) (SPLIT r' s') = do
  rr <- rConstraint r r'
  ss <- rConstraint s s'
  return $ rr ++ ss
rConstraint (ORD o) (ORD o') = do
  oo <- rConstraint o o'
  return oo
rConstraint (FUN f) (FUN f') = do
  ff <- rConstraint f f'
  return ff
rConstraint (LEFTSEC t s r) (LEFTSEC t' s' r') = do
  ss <- rConstraint s s'
  rr <- rConstraint r r'
  return $ [t :=: t'] ++ ss ++ rr
rConstraint (RIGHTSEC t s r) (RIGHTSEC t' s' r') = do
  ss <- rConstraint s s'
  rr <- rConstraint r r'
  return $ [t :=: t'] ++ ss ++ rr
rConstraint (APPLY t r v) (APPLY t' r' v') = do
  rr <- rConstraint r r'
  vv <- rConstraint v v'
  return $ [t :=: t'] ++ rr ++ vv
rConstraint (DEF n t) (DEF n' t') = do
  guard (n == n')
  return [t :=: t']
rConstraint (PROD r s) (PROD r' s') = do
  rr <- rConstraint r r'
  ss <- rConstraint s s'
  return $ rr ++ ss
rConstraint (EITHER r s) (EITHER r' s') = do
  rr <- rConstraint r r'
  ss <- rConstraint s s'
  return $ rr ++ ss
rConstraint (MAYBE r) (MAYBE r') = do
  rr <- rConstraint r r'
  return rr
rConstraint (LIST r) (LIST r') = do
  rr <- rConstraint r r'
  return rr
rConstraint (SET r) (SET r') = do
  rr <- rConstraint r r'
  return rr
rConstraint (MAP r) (MAP r') = do
  rr <- rConstraint r r'
  return rr
rConstraint (REYNOLDS r s) (REYNOLDS r' s') = do
  rr <- rConstraint r r'
  ss <- rConstraint s s'
  return $ rr ++ ss
rConstraint (FComp t f g) (FComp t' f' g') = do
  ff <- rConstraint f f'
  gg <- rConstraint g g'
  return $ [t :=: t'] ++ ff ++ gg
rConstraint (OComp r s) (OComp r' s') = do
  rr <- rConstraint r r'
  ss <- rConstraint s s'
  return $ rr ++ ss
rConstraint (OConv r) (OConv r') = do
  rr <- rConstraint r r'
  return rr
rConstraint (OJoin r) (OJoin r') = do
  rr <- rConstraint r r'
  return rr
rConstraint (OMax r) (OMax r') = do
  rr <- rConstraint r r'
  return rr
rConstraint (OMin r) (OMin r') = do
  rr <- rConstraint r r'
  return rr
rConstraint (GDef n f1 f2 o1 o2) (GDef n' f1' f2' o1' o2') = do
  guard (n == n')
  ff1 <- rConstraint f1 f1'
  ff2 <- rConstraint f2 f2'
  oo1 <- rConstraint o1 o1'
  oo2 <- rConstraint o2 o2'
  return $ ff1 ++ ff2 ++ oo1 ++ oo2
rConstraint (GComp t f g) (GComp t' f' g') = do
  ff <- rConstraint f f'
  gg <- rConstraint g g'
  return $ [t :=: t'] ++ ff ++ gg
rConstraint (GConv r) (GConv r') = do
  rr <- rConstraint r r'
  return rr
rConstraint (GLAdj r) (GLAdj r') = do
  rr <- rConstraint r r'
  return rr
rConstraint (GUAdj r) (GUAdj r') = do
  rr <- rConstraint r r'
  return rr
rConstraint (GLOrd t r) (GLOrd t' r') = do
  rr <- rConstraint r r'
  return $ [t :=: t'] ++ rr
rConstraint (GUOrd t r) (GUOrd t' r') = do
  rr <- rConstraint r r'
  return $ [t :=: t'] ++ rr
rConstraint f g =
  if req f g then return [] else mzero

-------------------------------------------------------------------------------
