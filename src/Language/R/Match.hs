
{-# LANGUAGE GADTs, PatternSignatures #-}
{-# OPTIONS_GHC -Wall #-}

-------------------------------------------------------------------------------

{- |
Module      :  Language.R.Match
Description :  Matching algorithm for expression representation.
Copyright   :  (c) Paulo Silva
License     :  LGPL
  
Maintainer  :  paufil@di.uminho.pt
Stability   :  experimental
Portability :  portable
 
-}
 
-------------------------------------------------------------------------------

module Language.R.Match (
  rMatch,
  rSubst
 ) where

import Control.MonadOr
import Control.Monad.Error
import Data.Maybe (listToMaybe)
import Language.R.Equality
import Language.R.Rewrite
import Language.R.SafeCast
import Language.R.Syntax
import Language.Type.Equality
import Language.Type.Syntax

-------------------------------------------------------------------------------

data Match where
  (:=<=:) :: R a -> R b -> Match

instance Show Match where
  show (r :=<=: r') = show "RMatch: " ++ show r ++ " =<= " ++ show r'

-------------------------------------------------------------------------------

rMatch :: MonadOr m => R a -> R b -> m [Match]
rMatch x@(Var _) r = return [x :=<=: r]
rMatch (NEG r) (NEG r') = do
  rr <- rMatch r r'
  return rr
rMatch (MEET r s) (MEET r' s') = do
  rr <- rMatch r r'
  ss <- rMatch s s'
  return $ rr ++ ss
rMatch (JOIN r s) (JOIN r' s') = do
  rr <- rMatch r r'
  ss <- rMatch s s'
  return $ rr ++ ss
rMatch (CONV r) (CONV r') = do
  rr <- rMatch r r'
  return rr
rMatch (COMP t f g) (COMP t' f' g') = do
  guard (beq t t')
  ff <- rMatch f f'
  gg <- rMatch g g'
  return $ ff ++ gg
rMatch (SPLIT r s) (SPLIT r' s') = do
  rr <- rMatch r r'
  ss <- rMatch s s'
  return $ rr ++ ss
rMatch (ORD o) (ORD o') = do
  oo <- rMatch o o'
  return oo
rMatch (FUN f) (FUN f') = do
  ff <- rMatch f f'
  return ff
rMatch (LEFTSEC t s r) (LEFTSEC t' s' r') = do
  guard (beq t t')
  ss <- rMatch s s'
  rr <- rMatch r r'
  return $ ss ++ rr
rMatch (RIGHTSEC t s r) (RIGHTSEC t' s' r') = do
  guard (beq t t')
  ss <- rMatch s s'
  rr <- rMatch r r'
  return $ ss ++ rr
rMatch (APPLY t r v) (APPLY t' r' v') = do
  guard (beq t t') 
  rr <- rMatch r r'
  vv <- rMatch v v'
  return $ rr ++ vv
rMatch (PROD r s) (PROD r' s') = do
  rr <- rMatch r r'
  ss <- rMatch s s'
  return $ rr ++ ss
rMatch (EITHER r s) (EITHER r' s') = do
  rr <- rMatch r r'
  ss <- rMatch s s'
  return $ rr ++ ss
rMatch (MAYBE r) (MAYBE r') = do
  rr <- rMatch r r'
  return rr
rMatch (LIST r) (LIST r') = do
  rr <- rMatch r r'
  return rr
rMatch (SET r) (SET r') = do
  rr <- rMatch r r'
  return rr
rMatch (MAP r) (MAP r') = do
  rr <- rMatch r r'
  return rr
rMatch (REYNOLDS r s) (REYNOLDS r' s') = do
  rr <- rMatch r r'
  ss <- rMatch s s'
  return $ rr ++ ss
rMatch (FComp t f g) (FComp t' f' g') = do
  guard (beq t t')
  ff <- rMatch f f'
  gg <- rMatch g g'
  return $ ff ++ gg
rMatch (OComp o1 o2) (OComp o1' o2') = do
  oo1 <- rMatch o1 o1'
  oo2 <- rMatch o2 o2'
  return $ oo1 ++ oo2
rMatch (OConv o) (OConv o') = do
  oo <- rMatch o o'
  return oo
rMatch (OProd o) (OProd o') = do
  oo <- rMatch o o'
  return oo
rMatch (OJoin o) (OJoin o') = do
  oo <- rMatch o o'
  return oo
rMatch (OMeet o) (OMeet o') = do
  oo <- rMatch o o'
  return oo
rMatch (OMax o) (OMax o') = do
  oo <- rMatch o o'
  return oo
rMatch (OMin o) (OMin o') = do
  oo <- rMatch o o'
  return oo
rMatch f g =
  if req f g then return [] else mzero

-------------------------------------------------------------------------------

type RuleSimpl = GenericM []

match2Rule :: Match -> RuleSimpl
match2Rule ((Var n) :=<=: r2) t (Var n') = do
  guard (n == n') 
  r2' <- rCast [] t r2
  return r2'
match2Rule _ _ _ = mzero

-------------------------------------------------------------------------------

rSubst :: [Match] -> Type a -> R a -> R a
rSubst mts t r = let (rs::[RuleSimpl]) = map match2Rule mts
  in maybe r id . listToMaybe $ (everywhere (try (seqRules rs))) t r

-------------------------------------------------------------------------------
