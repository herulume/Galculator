
{-# LANGUAGE GADTs, TypeOperators, FlexibleContexts, Rank2Types #-}
{-# OPTIONS_GHC -Wall #-}

-------------------------------------------------------------------------------

{- |
Module      :  Galculator.Engine.GcToLaw
Description :  
Copyright   :  (c) Paulo Silva
License     :  LGPL

Maintainer  :  paufil@di.uminho.pt
Stability   :  experimental
Portability :  portable

-}

-------------------------------------------------------------------------------

module Galculator.Engine.GcToLaw (
  --gc2Rules,
  rType2Law,
  gcProperties,
  gcPropertyFunc,
  gcShunting,
  gcDistributivityUpper,
  gcDistributivityLower,
  gcMonotonicityUpper,
  gcMonotonicityLower,
  gcPreservingBottom,
  gcPreservingTop,
  gcCancellationUpper,
  gcCancellationLower
 )  where

import Control.GalcError
import Control.Monad.Error
import Control.Monad.Fresh 
import Data.Existential
--import Galculator.Engine.LawToRule
--import Galculator.Rule
import Language.Law.Syntax
import Language.R.Syntax
import Language.Type.Syntax

-------------------------------------------------------------------------------
{-
gc2Rules :: Type (GC b a) -> R (GC b a) -> [Rule]
gc2Rules t gc = map getRule pp  ++
                map getRuleInv pp
  where pp = gcProperties t gc
-}
-------------------------------------------------------------------------------

rType2Law :: (MonadFresh [String] String m, MonadError GalcError m)
          => RType -> (forall a . Type a -> R a -> m Law) -> m Law
rType2Law (Exists t g) f = f t g

-------------------------------------------------------------------------------

gcProperties :: Type (GC b a) -> R (GC b a) -> [Law]
gcProperties t gc = map (\f -> f t gc) gcPropertyFunc

-------------------------------------------------------------------------------

gcPropertyFunc :: [Type (GC b a) -> R (GC b a) -> Law]
gcPropertyFunc = [] {- [
  gcShunting,
  --gcDistributivityUpper, 
  --gcDistributivityLower,
  gcMonotonicityUpper,
  gcMonotonicityLower, 
  gcPreservingTop,
  gcPreservingBottom, 
  gcCancellationUpper,
  gcCancellationLower
 ]-}

-------------------------------------------------------------------------------

gcShunting :: (MonadFresh [String] String m, MonadError GalcError m)
           => Type a -> R a -> m Law
gcShunting (GC b a) g = do
  ladj <- lowerAdjoint g
  uadj <- upperAdjoint g
  lord <- lowerOrder g
  uord <- upperOrder g
  return $ 
        EQUIV (Meta "Shunting" (Just GCSHUNT))
        (Rel a b)
        (COMP b (CONV ladj) (ORD lord))
        (COMP a (ORD uord) uadj)
gcShunting _ _ = throwError ImpossibleError

-------------------------------------------------------------------------------

gcDistributivityUpper :: (MonadFresh [String] String m, MonadError GalcError m)
                      => Type a -> R a -> m Law
gcDistributivityUpper (GC b a) g = do
  uadj <- upperAdjoint g
  lord <- lowerOrder g
  uord <- upperOrder g
  return $
        EQUIV (Meta ("Distributivity (Upper adjoint)") (Just GCDISTR) )
        (Rel a (Prod b b))
        (COMP b uadj (FUN (OMeet lord)))
        (COMP (Prod a a) (FUN (OMeet uord)) (PROD uadj uadj))
gcDistributivityUpper _ _ = error "gcDistributivityUpper"

-------------------------------------------------------------------------------

gcDistributivityLower :: (MonadFresh [String] String m, MonadError GalcError m)
                      => Type a -> R a -> m Law
gcDistributivityLower (GC b a) g = do
  ladj <- lowerAdjoint g
  lord <- lowerOrder g
  uord <- upperOrder g
  return $
        EQUIV (Meta ("Distributivity (Lower adjoint)") (Just GCDISTR) )
        (Rel b (Prod a a))
        (COMP a ladj (FUN (OJoin uord)))
        (COMP (Prod b b) (FUN (OJoin lord)) (PROD ladj ladj))
gcDistributivityLower _ _ = error "gcDistributivityLower"

-------------------------------------------------------------------------------

gcMonotonicityUpper :: (MonadFresh [String] String m, MonadError GalcError m)
                    => Type a -> R a -> m Law
gcMonotonicityUpper (GC b a) g = do
  uadj <- upperAdjoint g
  lord <- lowerOrder g
  uord <- upperOrder g
  return $
       IMPL (Meta "Monotonicity (upper adjoint)" (Just GCMONOT))
       (Rel a b)
       (COMP b uadj (ORD lord))
       (COMP a (ORD uord) uadj)
gcMonotonicityUpper _ _ = error "gcMonotonicityUpper"

-------------------------------------------------------------------------------

gcMonotonicityLower :: (MonadFresh [String] String m, MonadError GalcError m)
                    => Type a -> R a -> m Law
gcMonotonicityLower (GC b a) g = do
  ladj <- lowerAdjoint g
  lord <- lowerOrder g
  uord <- upperOrder g
  return $ 
       IMPL (Meta "Monotonicity (lower adjoint)" (Just GCMONOT))
       (Rel b a)
       (COMP a ladj (ORD uord))
       (COMP b (ORD lord) ladj)
gcMonotonicityLower _ _ = error "gcMonotonicityLower"

-------------------------------------------------------------------------------

gcPreservingTop :: (MonadFresh [String] String m, MonadError GalcError m)
                => Type a -> R a -> m Law
gcPreservingTop (GC b a) g = do
  uadj <- upperAdjointF g
  lord <- lowerOrder g
  uord <- upperOrder g
  return $
        EQUIV (Meta "Top preserving" (Just GCTOP))
        a
        (APPLY b uadj (OMax lord))
        (OMax uord)
gcPreservingTop _ _ = error "gcPreservingTop"

-------------------------------------------------------------------------------

gcPreservingBottom :: (MonadFresh [String] String m, MonadError GalcError m)
                   => Type a -> R a -> m Law
gcPreservingBottom (GC b a) g = do
  ladj <- lowerAdjointF g
  lord <- lowerOrder g
  uord <- upperOrder g
  return $
        EQUIV (Meta "Bottom preserving" (Just GCBOT))
        b
        (APPLY a ladj (OMin uord))
        (OMin lord)
gcPreservingBottom _ _ = error "gcPreservingBottom"

-------------------------------------------------------------------------------

gcCancellationUpper :: (MonadFresh [String] String m, MonadError GalcError m)
                    => Type a -> R a -> m Law
gcCancellationUpper (GC b a) g = do
  ladj <- lowerAdjoint g
  uadj <- upperAdjoint g
  uord <- upperOrder g
  return $
       IMPL (Meta "Cancellation (upper adjoint)" (Just GCCANC))
       (Rel a a)
       (ORD uord)
       (COMP a (ORD uord) (COMP b uadj ladj))
gcCancellationUpper _ _ = error "gcCancellationUpper"

-------------------------------------------------------------------------------

gcCancellationLower :: (MonadFresh [String] String m, MonadError GalcError m)
                    => Type a -> R a -> m Law
gcCancellationLower (GC b a) g = do
  ladj <- lowerAdjoint g
  uadj <- upperAdjoint g
  lord <- lowerOrder g
  return $
       IMPL (Meta "Cancellation (lowerAdjoint)" (Just GCCANC))
       (Rel b b)
       (COMP b (COMP a ladj uadj) (ORD lord))
       (ORD lord)
gcCancellationLower _ _ = error "gcCancellationLower"

-------------------------------------------------------------------------------

upperAdjoint :: MonadError GalcError m => R (GC b a) -> m (R (a :<->: b))
upperAdjoint (GDef _ _ g _ _) = return $ FUN g
upperAdjoint GId = return ID
upperAdjoint (GComp t g1 g2) = do
  g1' <- upperAdjoint g1
  g2' <- upperAdjoint g2
  return $ COMP t g2' g1'
upperAdjoint (GConv g) = lowerAdjoint g
upperAdjoint _ = throwError ImpossibleError

-------------------------------------------------------------------------------

lowerAdjoint :: MonadError GalcError m => R (GC b a) -> m (R (b :<->: a))
lowerAdjoint (GDef _ f _ _ _) = return $ FUN f
lowerAdjoint GId = return ID
lowerAdjoint (GComp t g1 g2) = do
  g1' <- lowerAdjoint g1 
  g2' <- lowerAdjoint g2
  return $ COMP t g1' g2'
lowerAdjoint (GConv g) = upperAdjoint g
lowerAdjoint _ = throwError ImpossibleError

-------------------------------------------------------------------------------

upperAdjointF :: MonadError GalcError m => R (GC b a) -> m (R (a :<-: b))
upperAdjointF (GDef _ _ g _ _) = return g
upperAdjointF GId = return FId 
upperAdjointF (GComp t g1 g2) = do
  g1' <- upperAdjointF g1
  g2' <- upperAdjointF g2
  return $ FComp t g2' g1'
upperAdjointF (GConv g) = lowerAdjointF g
upperAdjointF _ = throwError ImpossibleError

-------------------------------------------------------------------------------

lowerAdjointF :: MonadError GalcError m => R (GC b a) -> m (R (b :<-: a))
lowerAdjointF (GDef _ f _ _ _) = return f
lowerAdjointF GId = return FId
lowerAdjointF (GComp t g1 g2) = do
  g1' <- lowerAdjointF g1
  g2' <- lowerAdjointF g2
  return $ FComp t g1' g2'
lowerAdjointF (GConv g) = upperAdjointF g
lowerAdjointF _ = throwError ImpossibleError

-------------------------------------------------------------------------------

lowerOrder :: (MonadFresh [String] String m, MonadError GalcError m) 
           => R (GC b a) -> m (R (PO b))
lowerOrder (GDef _ _ _ o _) = return o
lowerOrder GId = do
  o <- getFresh 
  return $ Var ('o':o)
lowerOrder (GComp _ g1 _) = lowerOrder g1
lowerOrder (GConv g) = do
  g' <- upperOrder g
  return $ OConv g'
lowerOrder _ = error "lowerOrder"

-------------------------------------------------------------------------------

upperOrder :: (MonadFresh [String] String m, MonadError GalcError m) 
           => R (GC b a) -> m (R (PO a))
upperOrder (GDef _ _ _ _ o) = return o
upperOrder GId = do
  o <- getFresh
  return $ Var ('o':o)
upperOrder (GComp _ _ g2) = upperOrder g2
upperOrder (GConv g) = do
  g' <- lowerOrder g
  return $ OConv g'
upperOrder _ = error "upperOrder"

-------------------------------------------------------------------------------

