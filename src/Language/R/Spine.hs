
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall #-}

-------------------------------------------------------------------------------

{- |
Module      :  Language.R.Spine
Description :  Spine representation for the expression representation.
Copyright   :  (c) Paulo Silva
License     :  LGPL

Maintainer  :  paufil@di.uminho.pt
Stability   :  experimental
Portability :  portable

-}

-------------------------------------------------------------------------------

module Language.R.Spine (
 Typed((:|)),
 Spine(Constr,Ap),
 fromSpine,
 toSpine
 ) where

import Language.R.Syntax
import Language.Type.Syntax

-------------------------------------------------------------------------------

data Typed a where
  (:|) :: Type a -> R a -> Typed (R a)

data Spine a where
  Constr  :: a -> Spine a
  Ap      :: Spine (a -> b) -> Typed a -> Spine b

-------------------------------------------------------------------------------

fromSpine :: Spine a -> a
fromSpine (Constr c) = c
fromSpine (f `Ap` (_:| a)) = (fromSpine f) a

-------------------------------------------------------------------------------
toSpine :: Type a -> R a -> Spine (R a)
toSpine (Rel _ _) BOT = Constr BOT
toSpine (Rel _ _) TOP = Constr TOP
toSpine (Rel b a) (NEG r) = 
  (Constr NEG) `Ap` (Rel b a :| r)
toSpine (Rel b a) (MEET r s) =
  ((Constr MEET) `Ap` (Rel b a :| r)) `Ap` (Rel b a :| s)
toSpine (Rel b a) (JOIN r s) = 
  ((Constr JOIN) `Ap` (Rel b a :| r)) `Ap` (Rel b a :| s)
toSpine (Rel _ _) ID = Constr ID
toSpine (Rel b a)  (CONV r) =
  (Constr CONV) `Ap` (Rel a b :| r)
toSpine (Rel c a) (COMP b r s) = 
  ((Constr (COMP b)) `Ap` (Rel c b :| r)) `Ap` (Rel b a :| s)
toSpine (Rel (Prod c b) a) (SPLIT r s) =
  ((Constr SPLIT) `Ap` (Rel c a :| r)) `Ap` (Rel b a :| s)
toSpine (Rel a _) (ORD p) = 
  (Constr ORD) `Ap` (Ord a :| p)
toSpine (Rel b a) (FUN f) =
  (Constr FUN) `Ap` (Fun b a :| f)
toSpine (Fun a c) (LEFTSEC b f s) = 
  ((Constr (LEFTSEC b)) `Ap` (Fun a (Prod b c) :| f)) `Ap` (b :| s)
toSpine (Fun a b) (RIGHTSEC c f s) = 
  ((Constr (RIGHTSEC c)) `Ap` (Fun a (Prod b c) :| f)) `Ap` (c :| s)
toSpine a (APPLY b r v) = 
  ((Constr (APPLY b)) `Ap` (Fun a b :| r)) `Ap` (b :| v)
toSpine _ v@(DEF _ _) = Constr v
toSpine _ v@(Var _) = Constr v
toSpine (Rel (Prod d b) (Prod c a)) (PROD r s) = 
  ((Constr PROD) `Ap` (Rel d c :| r)) `Ap` (Rel b a :| s)
toSpine (Rel (Either d b) (Either c a)) (EITHER r s) = 
  ((Constr EITHER) `Ap` (Rel d c :| r)) `Ap` (Rel b a :| s)
toSpine (Rel (Maybe b) (Maybe a)) (MAYBE r) = 
  (Constr MAYBE) `Ap` (Rel b a :| r)
toSpine (Rel (List b) (List a)) (LIST r) = 
  (Constr LIST) `Ap` (Rel b a :| r)
toSpine (Rel (Set b) (Set a)) (SET r) = 
  (Constr SET) `Ap` (Rel b a :| r)
toSpine (Rel (Map _ b) (Map _ a)) (MAP r) = 
  (Constr MAP) `Ap` (Rel b a :| r)
toSpine (Rel (Fun b d) (Fun a c)) (REYNOLDS r s) = 
  ((Constr REYNOLDS) `Ap` (Rel b a :| r)) `Ap` (Rel d c :| s)
toSpine _ FId = Constr FId
toSpine (Fun c a) (FComp b f g) = 
  ((Constr (FComp b)) `Ap` (Fun c b :| f)) `Ap` (Fun b a :| g)
toSpine (Ord _) OId = Constr OId 
toSpine (Ord a) (OComp o1 o2) =
  ((Constr OComp) `Ap` (Ord a :| o1)) `Ap` (Ord a :| o2)
toSpine (Ord a) (OConv o) = 
  (Constr OConv) `Ap` (Ord a :| o)
toSpine (Ord (Prod a _)) (OProd o) =
  (Constr OProd) `Ap` (Ord a :| o)
toSpine (Fun a (Prod _ _)) (OJoin o) = 
  (Constr OJoin) `Ap` (Ord a :| o)
toSpine (Fun a (Prod _ _)) (OMeet o) = 
  (Constr OMeet) `Ap` (Ord a :| o)
toSpine a (OMax o) = 
  (Constr OMax) `Ap` (Ord a :| o)
toSpine a (OMin o) =
  (Constr OMin) `Ap` (Ord a :| o)
-- Galois connections
toSpine (GC b a) (GDef name f g fo go) =
  ((((Constr (GDef name)) `Ap` 
  (Fun b a :| f)) `Ap` 
  (Fun a b :| g)) `Ap` 
  (Ord b :| fo)) `Ap` 
  (Ord a :| go)
toSpine (GC _ _) GId = Constr GId
toSpine (GC c a) (GComp b g1 g2) = 
  ((Constr (GComp b)) `Ap` (GC c b :| g1)) `Ap` (GC b a :| g2)
toSpine (GC a b) (GConv g) = 
  (Constr GConv) `Ap` (GC b a :| g)
toSpine (Fun b a) (GLAdj f) = 
  (Constr GLAdj) `Ap` (GC b a :| f)
toSpine (Fun a b) (GUAdj f) = 
  (Constr GUAdj) `Ap` (GC b a :| f)
toSpine (Ord b) (GLOrd a o) = 
  (Constr (GLOrd a)) `Ap` (GC b a :| o)
toSpine (Ord a) (GUOrd b o) = 
  (Constr (GUOrd b)) `Ap` (GC b a :| o)
-- Missing cases
toSpine t a = error $ "toSpine missing case for " ++ show t ++ " :| " ++ show a
-------------------------------------------------------------------------------
