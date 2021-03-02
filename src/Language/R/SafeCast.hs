
{-# LANGUAGE GADTs, FlexibleContexts, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wall #-}

-------------------------------------------------------------------------------

{- |
Module      :  Language.R.SafeCast
Description :  Type safe cast for the expression representation.
Copyright   :  (c) Paulo Silva
License     :  LGPL
  
Maintainer  :  paufil@di.uminho.pt
Stability   :  experimental
Portability :  portable
 
-}

-------------------------------------------------------------------------------

module Language.R.SafeCast (
 rCast
 ) where

import Control.GalcError
import Control.Monad.Error
import Data.Equal
import Data.Existential
import Language.R.Syntax
import Language.Type.Constraint
import Language.Type.Equality
import Language.Type.Syntax

-------------------------------------------------------------------------------

rCast :: MonadError GalcError m 
      => [Constraint] -> Type t -> R r -> m (R t)
rCast _ (Rel _ _) BOT = return BOT
rCast _ (Rel _ _) TOP = return TOP
rCast c (Rel t1 t2) (NEG r) = do
  r' <- rCast c (Rel t1 t2) r
  return $ NEG r'
rCast c (Rel t1 t2) (MEET r1 r2) = do
  r1' <- rCast c (Rel t1 t2) r1 
  r2' <- rCast c (Rel t1 t2) r2
  return $ MEET r1' r2'
rCast c (Rel t1 t2) (JOIN r1 r2) = do
  r1' <- rCast c (Rel t1 t2) r1
  r2' <- rCast c (Rel t1 t2) r2
  return $ JOIN r1' r2'
rCast _ (Rel t1 t2) ID = do
  Eq <- teqE t1 t2
  return ID
rCast c (Rel t1 t2) (CONV r1) = do
  r1' <- rCast c (Rel t2 t1) r1
  return $ CONV r1'
rCast c (Rel t1 t2) (COMP t r1 r2) = do
  Hide  t' <- typeRewriteE c t
  r1' <- rCast c (Rel t1 t') r1
  r2' <- rCast c (Rel t' t2) r2
  return $ COMP t' r1' r2'
rCast c (Rel (Prod t2 t3) t1) (SPLIT r1 r2) = do
  r1' <- rCast c (Rel t2 t1) r1
  r2' <- rCast c (Rel t3 t1) r2
  return $ SPLIT r1' r2'
rCast c (Rel t1 t2) (ORD r) = do
  Eq <- teqE t1 t2
  r' <- rCast c (Ord t1) r
  return $ ORD r'
rCast c (Rel t2 t1) (FUN f) = do
  f' <- rCast c (Fun t2 t1) f
  return $ FUN f'
rCast c (Fun t1 t3) (LEFTSEC t2 f s) = do
  Hide t2' <- typeRewriteE c t2
  f' <- rCast c (Fun t1 (Prod t2' t3)) f
  s' <- rCast c t2' s
  return $ LEFTSEC t2' f' s'
rCast c (Fun t1 t2) (RIGHTSEC t3 f s) = do
  Hide t3' <- typeRewriteE c t3
  f' <- rCast c (Fun t1 (Prod t2 t3')) f
  s' <- rCast c t3' s
  return $ RIGHTSEC t3' f' s'
rCast c t1 (APPLY t2 f v) = do
  Hide t2' <- typeRewriteE c t2
  v' <- rCast c t2' v
  f' <- rCast c (Fun t1 t2') f
  return $ APPLY t2' f' v'
rCast c ta (DEF n tb) = do
  Hide tb' <- typeRewriteE c tb
  Eq <- teqE tb' ta
  return $ DEF n ta
rCast _ _ (Var n) = return (Var n)
rCast c (Rel (Prod t2' t1') (Prod t2 t1)) (PROD r2 r1) = do
  r1' <- rCast c (Rel t1' t1) r1
  r2' <- rCast c (Rel t2' t2) r2
  return $ PROD r2' r1'
rCast c (Rel (Either t2' t1') (Either t2 t1)) (EITHER r2 r1) = do
  r1' <- rCast c (Rel t1' t1) r1
  r2' <- rCast c (Rel t2' t2) r2
  return $ EITHER r2' r1'
rCast c (Rel (Maybe b) (Maybe a)) (MAYBE r) = do
  r' <- rCast c (Rel b a) r
  return $ MAYBE r'
rCast c (Rel (List b) (List a)) (LIST r) = do
  r' <- rCast c (Rel b a) r
  return $ LIST r'
rCast c (Rel (Set b) (Set a)) (SET r) = do
  r' <- rCast c (Rel b a) r
  return $ SET r'
rCast c (Rel (Map k b) (Map k' a)) (MAP r) = do
  Eq <- teqE k k'
  r' <- rCast c (Rel b a) r
  return $ MAP r'
rCast c (Rel (Fun t2 t4) (Fun t1 t3)) (REYNOLDS r2 r1) = do
  r1' <- rCast c (Rel t4 t3) r1
  r2' <- rCast c (Rel t2 t1) r2
  return $ REYNOLDS r2' r1'
rCast _ (Fun t1 t2) FId = do 
  Eq <- teqE t1 t2
  return FId
rCast c (Fun t1 t3) (FComp t2 f1 f2) = do
  Hide t2' <- typeRewriteE c t2
  f1' <- rCast c (Fun t1 t2') f1
  f2' <- rCast c (Fun t2' t3) f2
  return $ FComp t2' f1' f2'
rCast _ (Ord _) OId = return OId
rCast c (Ord t) (OComp o1 o2) = do
  o1' <- rCast c (Ord t) o1
  o2' <- rCast c (Ord t) o2
  return $ OComp o1' o2'
rCast c (Ord t) (OConv o) = do
  o' <- rCast c (Ord t) o
  return $ OConv o'
rCast c (Ord (Prod a a')) (OProd o) = do
  Eq <- teqE a a'
  o' <- rCast c (Ord a) o
  return $ OProd o'
rCast c (Fun t1 (Prod t2 t3)) (OJoin o) = do
  Eq <- teqE t1 t2
  Eq <- teqE t2 t3
  o' <- rCast c (Ord t1) o
  return $ OJoin o'
rCast c (Fun t1 (Prod t2 t3)) (OMeet o) = do
  Eq <- teqE t1 t2
  Eq <- teqE t2 t3
  o' <- rCast c (Ord t1) o
  return $ OMeet o'
rCast c t (OMax o) = do
  o' <- rCast c (Ord t) o
  return $ OMax o'
rCast c t (OMin o) = do
  o' <- rCast c (Ord t) o
  return $ OMin o'
rCast c (GC t1 t2) (GDef n f1 f2 o1 o2) = do
  f1' <- rCast c (Fun t1 t2) f1
  f2' <- rCast c (Fun t2 t1) f2
  o1' <- rCast c (Ord t1) o1
  o2' <- rCast c (Ord t2) o2
  return $ GDef n f1' f2' o1' o2'
rCast _ (GC t1 t2) GId = do
  Eq <- teqE t1 t2 
  return $ GId
rCast c (GC t1 t3) (GComp t2 g1 g2) = do
  Hide t2' <- typeRewriteE c t2
  g1' <- rCast c (GC t1 t2') g1
  g2' <- rCast c (GC t2' t3) g2
  return $ GComp t2' g1' g2'
rCast c (GC t1 t2) (GConv g) = do
  g' <- rCast c (GC t2 t1) g
  return $ GConv g'
rCast c (Fun t1 t2) (GLAdj g) = do
  g' <- rCast c (GC t1 t2) g
  return $ GLAdj g'
rCast c (Fun t1 t2) (GUAdj g) = do
  g' <- rCast c (GC t2 t1) g
  return $ GUAdj g'
rCast c (Ord t1) (GLOrd t2 g) = do
  Hide t2' <- typeRewriteE c t2
  g' <- rCast c (GC t1 t2') g
  return $ GLOrd t2' g'
rCast c (Ord t1) (GUOrd t2 g) = do
  Hide t2' <- typeRewriteE c t2
  g' <- rCast c (GC t2' t1) g
  return $ GUOrd t2' g'
rCast _ t r = throwError $ CastingError (show r, show t)

-------------------------------------------------------------------------------
