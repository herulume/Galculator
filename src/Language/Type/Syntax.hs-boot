
{-# LANGUAGE GADTs, TypeOperators, EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall #-}

-------------------------------------------------------------------------------

{- |
Module      :  Language.Type.Syntax
Description :
Copyright   :  (c) Paulo Silva
License     :  LGPL

Maintainer  :  paufil@di.uminho.pt
Stability   :  experimental
Portability :  portable

<description of the module>
-}

-------------------------------------------------------------------------------

module Language.Type.Syntax (
  Type(..),
  (:<->:),
  (:<-:),
  PO,
  Var,
  GC,
  One,
  TypeBox
 ) where

import Data.Existential
import Data.Map
import Data.Set


-------------------------------------------------------------------------------

type One = ()
type Name = String
type b :<-: a = a -> b

data b :<->: a
data GC b a
data PO a
data Var

-------------------------------------------------------------------------------
data Type a where
  TVar   :: Name -> Type Var

  One    :: Type One
  Bool   :: Type Bool
  Char   :: Type Char
  String :: Type String
  Int    :: Type Int
  Float  :: Type Float

  Prod   :: Type a -> Type b -> Type (a,b)
  Either :: Type a -> Type b -> Type (Either a b)

  Maybe  :: Type a -> Type (Maybe a)
  List   :: Type a -> Type [a]
  Set    :: Type a -> Type (Set a)
  Map    :: Type a -> Type b -> Type (Map a b)

  Fun    :: Type b -> Type a -> Type (b :<-: a)
  Rel    :: Type b -> Type a -> Type (b :<->: a)
  Ord    :: Type a -> Type (PO a)

  GC     :: Type b -> Type a -> Type (GC b a)

-------------------------------------------------------------------------------

type TypeBox = Covert Type
 
-------------------------------------------------------------------------------

