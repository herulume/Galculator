
{-# OPTIONS_GHC -Wall #-}
 
-------------------------------------------------------------------------------

{- |
Module      :  Language.Type.Utils
Description :  Useful functions for dealing with type representation.
Copyright   :  (c) Paulo Silva
License     :  LGPL
  
Maintainer  :  paufil@di.uminho.pt
Stability   :  experimental
Portability :  portable
 
-}

-------------------------------------------------------------------------------
  
module Language.Type.Utils (
  isBasicType,
  isBasicTypeTB,
  isTVar,
  isTVarTB,
  occursIn,
  occursInTB,
  isConstructor,
  collectTVar,
  collectTVarTB,
  getTVarNameTB
 ) where

import Data.Existential
import Language.Type.Syntax

-------------------------------------------------------------------------------

isBasicType :: Type a -> Bool
isBasicType One = True
isBasicType Bool = True
isBasicType Char = True
isBasicType String = True
isBasicType Int = True
isBasicType Float = True
isBasicType _ = False

-------------------------------------------------------------------------------

isBasicTypeTB :: TypeBox -> Bool
isBasicTypeTB (Hide t) = isBasicType t

-------------------------------------------------------------------------------

isTVar :: Type a -> Bool
isTVar (TVar _) = True
isTVar _ = False

-------------------------------------------------------------------------------

isTVarTB :: TypeBox -> Bool
isTVarTB (Hide t) = isTVar t

-------------------------------------------------------------------------------

occursIn :: Type a -> Type b -> Bool
occursIn t (Prod a b) = occursIn t a || occursIn t b
occursIn t (Either a b) = occursIn t a || occursIn t b
occursIn t (Maybe a) = occursIn t a
occursIn t (List a) = occursIn t a
occursIn t (Set a) = occursIn t a
occursIn t (Map a b) = occursIn t a || occursIn t b
occursIn t (Fun a b) = occursIn t a || occursIn t b
occursIn t (Rel a b) = occursIn t a || occursIn t b
occursIn t (Ord a) = occursIn t a
occursIn t (GC a b) = occursIn t a || occursIn t b
occursIn (TVar n) (TVar m) = n == m
occursIn _ _ = False

-------------------------------------------------------------------------------

occursInTB :: TypeBox -> TypeBox -> Bool
occursInTB (Hide a) (Hide b) = a `occursIn` b

-------------------------------------------------------------------------------

isConstructor :: Type a -> Bool
isConstructor t = not (isBasicType t) && not (isTVar t)

-------------------------------------------------------------------------------

collectTVar :: Type a -> [TypeBox]
collectTVar x@(TVar _) = [Hide x]
collectTVar (Prod a b) = collectTVar a ++ collectTVar b
collectTVar (List a) = collectTVar a
collectTVar (Either a b) = collectTVar a ++ collectTVar b
collectTVar (Maybe a) = collectTVar a
collectTVar (Set a) = collectTVar a
collectTVar (Ord a) = collectTVar a
collectTVar (Map a b) = collectTVar a ++ collectTVar b
collectTVar (Fun a b) = collectTVar a ++ collectTVar b
collectTVar (GC a b) = collectTVar a ++ collectTVar b
collectTVar _ = []

-------------------------------------------------------------------------------

collectTVarTB :: TypeBox -> [TypeBox]
collectTVarTB (Hide t) = collectTVar t

-------------------------------------------------------------------------------

getTVarNameTB :: TypeBox -> String
getTVarNameTB (Hide (TVar n)) = n
getTVarNameTB _ = ""

-------------------------------------------------------------------------------
