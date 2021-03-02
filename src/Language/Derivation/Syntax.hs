

{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall #-}

-------------------------------------------------------------------------------

{- |
Module      :  Language.Derivation.Syntax
Description :  Abstract representation of derivation rules.
Copyright   :  (c) Paulo Silva
License     :  LGPL
  
Maintainer  :  paufil@di.uminho.pt
Stability   :  experimental
Portability :  portable
  
-}
  
-------------------------------------------------------------------------------

module Language.Derivation.Syntax (
  Derivation(..),
  derivations
 ) where

-------------------------------------------------------------------------------

data Derivation where
  Inv        :: Derivation -> Derivation
  Shunt      :: String -> Derivation
  DistrLow   :: String -> Derivation
  DistrUp    :: String -> Derivation
  MonotLow   :: String -> Derivation
  MonotUp    :: String -> Derivation
  TopPreserv :: String -> Derivation
  BotPreserv :: String -> Derivation
  CancUp     :: String -> Derivation
  CancLow    :: String -> Derivation
  Free       :: String -> Derivation
  Apply      :: String -> Derivation
  deriving Show
 
-------------------------------------------------------------------------------

derivations :: [String]
derivations = [
  "inv", "shunt", "distr_low", "distr_up", "monot_low", "monot_up",
  "top_preserving", "bot_preserving", "canc_up", "canc_low", "free", "apply"
 ]

-------------------------------------------------------------------------------