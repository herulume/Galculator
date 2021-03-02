
{-# OPTIONS_GHC -Wall #-}

-------------------------------------------------------------------------------

{- |
Module      :  Language.R.Equality
Description :  Equality over the expression representations.
Copyright   :  (c) Paulo Silva
License     :  LGPL

Maintainer  :  paufil@di.uminho.pt
Stability   :  experimental
Portability :  portable

-}
 
-------------------------------------------------------------------------------
 
 
module Language.R.Equality (
  req
 ) where

import {-# SOURCE #-} Language.R.Syntax
import Language.Type.Equality

-------------------------------------------------------------------------------
req :: R a -> R b -> Bool
req BOT BOT = True
req TOP TOP = True
req (NEG r) (NEG r') = req r r'
req (MEET r s) (MEET r' s') = 
  req r r' && req s s'
req (JOIN r s) (JOIN r' s') = 
  req r r' && req s s'
req ID ID = True
req (CONV r) (CONV r') = req r r'
req (COMP b r s) (COMP b' r' s') =
  beq b b' &&  req r r' && req s s'
req (SPLIT r s) (SPLIT r' s') =
  req r r' && req s s'
req (ORD o) (ORD o') = req o o'
req (FUN f) (FUN f') = req f f'
req (LEFTSEC t f s) (LEFTSEC t' f' s') =
  beq t t' && req f f' && req s s'
req (RIGHTSEC t f s) (RIGHTSEC t' f' s') =
  beq t t' && req f f' && req s s'
req (APPLY t f v) (APPLY t' f' v') =
  beq t t' && req f f' && req v v'
req (DEF n t) (DEF n' t') = 
  n == n' && beq t t'
req (Var n) (Var n') = n == n'

req (PROD r s) (PROD r' s') = 
  req r r' && req s s'
req (EITHER r s) (EITHER r' s') = 
  req r r' && req s s'
req (MAYBE r) (MAYBE r') = req r r'
req (LIST r) (LIST r') = req r r'
req (SET r) (SET r') = req r r'
req (MAP r) (MAP r') = req r r'
req (REYNOLDS r s) (REYNOLDS r' s') = 
  req r r' && req s s'
req FId FId = True
req (FComp t f g) (FComp t' f' g') = 
  beq t t' && req f f' && req g g'

req OId OId = True
req (OComp o1 o2) (OComp o1' o2') =
  req o1 o1' && req o2 o2'
req (OConv o) (OConv o') = req o o'
req (OProd o) (OProd o') = req o o'
req (OJoin o) (OJoin o') = req o o'
req (OMeet o) (OMeet o') = req o o'
req (OMax o) (OMax o') = req o o'
req (OMin o) (OMin o') = req o o'

req (GDef n f g fo go) (GDef n' f' g' fo' go') =
  n == n' && req f f' && req g g' && req fo fo' && req go go'
req GId GId = True
req (GComp t g1 g2) (GComp t' g1' g2') = 
  beq t t' && req g1 g1' && req g2 g2'
req (GConv g) (GConv g') = req g g'
req (GLAdj g) (GLAdj g') = req g g'
req (GUAdj g) (GUAdj g') = req g g'
req (GLOrd t g) (GLOrd t' g') = 
  beq t t' && req g g'
req (GUOrd t g) (GUOrd t' g') = 
  beq t t' && req g g'
req _ _ = False

-------------------------------------------------------------------------------

