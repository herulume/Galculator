
{-# LANGUAGE GADTs, TypeOperators, EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall #-}

-------------------------------------------------------------------------------

{- |
Module      :  Language.Type.Syntax
Description :  Polymorphic type representation of the types used by Galculator. 
Copyright   :  (c) Paulo Silva
License     :  LGPL

Maintainer  :  paufil@di.uminho.pt
Stability   :  experimental
Portability :  portable

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
  Name,
  TypeBox
 ) where

import Data.Existential
import Data.Map
import Data.Set
import Language.Type.Equality
import Language.Type.Pretty

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

instance Show (Type t) where
  showsPrec _ = showType

-------------------------------------------------------------------------------

instance Eq (Type a) where
  t1 == t2 = beq t1 t2

-------------------------------------------------------------------------------

type TypeBox = Covert Type
 
-------------------------------------------------------------------------------
  
instance Eq TypeBox where
  (Hide t) == (Hide t') = beq t t'
  
-------------------------------------------------------------------------------

instance Show TypeBox where
  show (Hide a) = show a
  
-------------------------------------------------------------------------------

