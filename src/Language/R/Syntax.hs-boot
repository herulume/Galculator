
{-# LANGUAGE GADTs, TypeOperators, TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall #-}

-------------------------------------------------------------------------------

{- |
Module      :  Language.R.Syntax
Description :  
Copyright   :  (c) Paulo Silva
License     :  LGPL

Maintainer  :  paufil@di.uminho.pt
Stability   :  experimental
Portability :  portable

<description of the module>
-}

-------------------------------------------------------------------------------

module Language.R.Syntax (
 R(..),
 RType
 ) where

import Data.Existential
import Data.Map(Map)
import Data.Set(Set)
import Language.Type.Syntax

-------------------------------------------------------------------------------

data R r where
  BOT      :: R (b :<->: a)
  TOP      :: R (b :<->: a)
  NEG      :: R (b :<->: a) -> R (b :<->: a)
  MEET     :: R (b :<->: a) -> R (b :<->: a) -> R (b :<->: a)
  JOIN     :: R (b :<->: a) -> R (b :<->: a) -> R (b :<->: a)
  ID       :: R (a :<->: a)
  CONV     :: R (b :<->: a) -> R (a :<->: b)
  COMP     :: Type b -> R (c :<->: b) -> R (b :<->: a) -> R (c :<->: a)
  SPLIT    :: R (b :<->: a) -> R (c :<->: a) -> R ((b,c) :<->: a)
  
  ORD      :: R (PO a) -> R (a :<->: a)
  FUN      :: R (b :<-: a) -> R (b :<->: a)

  LEFTSEC  :: Type b -> R (a :<-: (b,c)) -> R b -> R (a :<-: c)
  RIGHTSEC :: Type c -> R (a :<-: (b,c)) -> R c -> R (a :<-: b)

  APPLY    :: Type b -> R (a :<-: b) -> R b -> R a
  DEF      :: Name -> Type a -> R a
  Var      :: Name -> R a

  PROD     :: R (b :<->: a) -> R (d :<->: c) -> R ((b,d) :<->: (a,c))
  EITHER   :: R (b :<->: a) -> R (d :<->: c) -> R (Either b d :<->: Either a c)
  MAYBE    :: R (b :<->: a) -> R (Maybe b :<->: Maybe a)
  LIST     :: R (b :<->: a) -> R ([b] :<->: [a])
  SET      :: R (b :<->: a) -> R (Set b :<->: Set a)
  MAP      :: R (b :<->: a) -> R (Map k b :<->: Map k a)

  REYNOLDS :: R (b :<->: a) -> R (d :<->: c) -> R ((b :<-: d) :<->: (a :<-: c))

  FId      :: R (a :<-: a)
  FComp    :: Type b -> R (c :<-: b) -> R (b :<-: a) -> R (c :<-: a)

  OId      :: R (PO a)
  OComp    :: R (PO a) -> R (PO a) -> R (PO a)
  OConv    :: R (PO a) -> R (PO a)
  OProd    :: R (PO a) -> R (PO (a,a))

  OJoin    :: R (PO a) -> R (a :<-: (a,a))
  OMeet    :: R (PO a) -> R (a :<-: (a,a))
  OMax     :: R (PO a) -> R a
  OMin     :: R (PO a) -> R a

  GDef     :: Name
           -> R (b :<-: a) -> R (a :<-: b)
           -> R (PO b) -> R (PO a)
           -> R (GC b a)

  GId      :: R (GC a a)
  GComp    :: Type b -> R (GC c b) -> R (GC b a) -> R (GC c a)
  GConv    :: R (GC b a) -> R (GC a b)

  GLAdj    :: R (GC b a) -> R (b :<-: a)
  GUAdj    :: R (GC b a) -> R (a :<-: b)
  GLOrd    :: Type a -> R (GC b a) -> R (PO b)
  GUOrd    :: Type b -> R (GC b a) -> R (PO a)

-------------------------------------------------------------------------------

type RType = Exists Type R

-------------------------------------------------------------------------------
