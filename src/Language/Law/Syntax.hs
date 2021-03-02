
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall #-}

-------------------------------------------------------------------------------

{- |
Module      :  Language.Law.Syntax
Description :  
Copyright   :  (c) Paulo Silva
License     :  LGPL

Maintainer  :  paufil@di.uminho.pt
Stability   :  experimental
Portability :  portable

<description of the module>
-}

-------------------------------------------------------------------------------

module Language.Law.Syntax (
  Law(..),
  RuleType(..),
  Meta(..),
  getName,
  getLeft,
  getRight
 ) where

import Data.Existential
import Language.R.Syntax
import Language.Type.Syntax

-------------------------------------------------------------------------------

data Law where
  EQUIV :: Meta -> Type a -> R a -> R a -> Law
  IMPL  :: Meta -> Type a -> R a -> R a -> Law

-------------------------------------------------------------------------------

instance Show Law where
  show (EQUIV m _ r r') = 
    show r ++ " <=> " ++ show r' ++ " , " ++ show m
  show (IMPL m _ r r') = 
    show r ++ " => " ++ show r' ++ " , " ++ show m
{-
  show (EQUIV m _ r r') =
    "EQUIV " ++ (name m) ++ " " ++ show r ++ " " ++ show r'
  show (IMPL m _ r r') =
    "IMPL " ++ (name m) ++ " " ++ show r ++ " " ++ show r'
-}
-------------------------------------------------------------------------------

data RuleType =
   ASSOC
 | COMUT
 | DISTR
 | UNIV
 | IDEMP
 | INVOL
 | UNIT
 | CONTRAV
 | GCSHUNT
 | GCCANC
 | GCMONOT
 | GCDISTR
 | GCTOP
 | GCBOT
 | DEFINITION
 | FUSION
 | ASSUMP
 | RuleType String

-------------------------------------------------------------------------------

instance Show RuleType where
  show ASSOC = "Associativity"
  show COMUT = "Comutativity"
  show DISTR = "Distributivity"
  show UNIV = "Universal Property"
  show IDEMP = "Idempotence"
  show INVOL = "Involution"
  show UNIT = "Unit"
  show CONTRAV = "Contravariance"
  show GCSHUNT = "Shunting"
  show GCCANC = "Cancellation"
  show GCMONOT = "Monotonic"
  show GCDISTR = "Distributivity"
  show GCTOP = "Top-preserving"
  show GCBOT = "Bottom-preserving"
  show DEFINITION = "Definition"
  show FUSION = "Fusion"
  show ASSUMP = "Assumption"
  show (RuleType nm) = nm

-------------------------------------------------------------------------------

data Meta = Meta {
  name :: String,
  ruleType :: Maybe RuleType
 }

-------------------------------------------------------------------------------

instance Show Meta where
  show (Meta n (Just tp)) =
    "{[" ++ n ++ ": " ++ show tp ++ "]}"
  show (Meta n Nothing) =
    "{[" ++ n ++ "]}"

-------------------------------------------------------------------------------

getName :: Law -> String
getName (EQUIV m _ _ _) = name m
getName (IMPL m _ _ _)  = name m

-------------------------------------------------------------------------------
-- | Returns the left expression of a law. 
-- Refactor?
getLeft :: Law -> RType
getLeft (EQUIV _ t r _) = Exists t r
getLeft (IMPL _ t r _) = Exists t r

-------------------------------------------------------------------------------
-- | Returns the right expression of a law.
getRight :: Law -> RType 
getRight (EQUIV _ t _ r) = Exists t r
getRight (IMPL _ t _ r) = Exists t r

-------------------------------------------------------------------------------

