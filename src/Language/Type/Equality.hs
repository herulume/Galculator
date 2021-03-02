
{-# LANGUAGE GADTs, FlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}

-------------------------------------------------------------------------------

{- |
Module      :  Language.Type.Equality
Description :  Equality definition over the type representation. 
Copyright   :  (c) Paulo Silva
License     :  LGPL

Maintainer  :  paufil@di.uminho.pt
Stability   :  experimental
Portability :  portable

-}
 
-------------------------------------------------------------------------------
 
 
module Language.Type.Equality (
  teq,
  teqE,
  teq',
  beq,
  beq'
 ) where

import Control.GalcError
import Control.MonadOr
import Control.Monad.Error
import Data.Equal
import {-# SOURCE #-} Language.Type.Syntax

-------------------------------------------------------------------------------

teqE :: MonadError GalcError m => Type a -> Type b -> m (Equal a b)
teqE t1 t2 = maybe2error EqualityError  $ teq t1 t2

teq :: MonadOr m => Type a -> Type b -> m (Equal a b)
teq (TVar n1) (TVar n2) = do
  guard (n1 == n2)
  return Eq 
teq One One = return Eq
teq Bool Bool = return Eq
teq Char Char = return Eq 
teq String String = return Eq
teq Int Int = return Eq
teq Float Float = return Eq
teq (Prod a b) (Prod a' b') = do
  Eq <- teq a a'
  Eq <- teq b b'
  return Eq
teq (Either a b) (Either a' b') = do
  Eq <- teq a a'
  Eq <- teq b b'
  return Eq
teq (Maybe a) (Maybe a') = do
  Eq <- teq a a'
  return Eq
teq (List a) (List a') = do
  Eq <- teq a a'
  return Eq
teq (Set a) (Set a') = do
  Eq <- teq a a'
  return Eq
teq (Map a b) (Map a' b') = do
  Eq <- teq a a'
  Eq <- teq b b'
  return Eq
teq (Fun b a) (Fun b' a') = do
  Eq <- teq a a'
  Eq <- teq b b'
  return Eq
teq (Rel b a) (Rel b' a') = do
  Eq <- teq a a'
  Eq <- teq b b'
  return Eq
teq (Ord a) (Ord a') = do
  Eq <- teq a a'
  return Eq
teq (GC b a) (GC b' a') = do
  Eq <- teq a a'
  Eq <- teq b b'
  return Eq
teq _ _ = mzero

-------------------------------------------------------------------------------

teq' :: MonadPlus m => Type a -> Type b -> m (Equal a b)
teq' (TVar _) (TVar _) = do
  return Eq
teq' One One = return Eq
teq' Bool Bool = return Eq
teq' Char Char = return Eq
teq' String String = return Eq
teq' Int Int = return Eq
teq' Float Float = return Eq
teq' (Prod a b) (Prod a' b') = do
  Eq <- teq' a a'
  Eq <- teq' b b'
  return Eq
teq' (Either a b) (Either a' b') = do
  Eq <- teq' a a'
  Eq <- teq' b b'
  return Eq
teq' (Maybe a) (Maybe a') = do
  Eq <- teq' a a'
  return Eq
teq' (List a) (List a') = do
  Eq <- teq' a a'
  return Eq
teq' (Set a) (Set a') = do
  Eq <- teq' a a'
  return Eq
teq' (Map a b) (Map a' b') = do
  Eq <- teq' a a'
  Eq <- teq' b b'
  return Eq
teq' (Fun b a) (Fun b' a') = do
  Eq <- teq' a a'
  Eq <- teq' b b'
  return Eq
teq' (Rel b a) (Rel b' a') = do
  Eq <- teq' a a'
  Eq <- teq' b b'
  return Eq
teq' (Ord a) (Ord a') = do
  Eq <- teq' a a'
  return Eq
teq' (GC b a) (GC b' a') = do
  Eq <- teq' a a'
  Eq <- teq' b b'
  return Eq
teq' _ _ = mzero

-------------------------------------------------------------------------------

beq :: Type a -> Type b -> Bool
beq a a' = maybe False (const True) (teq a a')

-------------------------------------------------------------------------------

beq' :: Type a -> Type b -> Bool
beq' a a' = maybe False (const True) (teq' a a')

-------------------------------------------------------------------------------
