
{-# OPTIONS_GHC -Wall #-}

-------------------------------------------------------------------------------

{- |
Module      :  Language.Module.SyntaxADT
Description :  Representation of the modules used by Galculator, using 
               Abstract Data Types 
Copyright   :  (c) Paulo Silva
License     :  LGPL

Maintainer  :  paufil@di.uminho.pt
Stability   :  experimental
Portability :  portable

-}

-------------------------------------------------------------------------------

module Language.Module.SyntaxADT (
  ModuleS(..)
 ) where

import Language.Law.SyntaxADT
import Language.R.SyntaxADT

-------------------------------------------------------------------------------

data ModuleS = ModuleS {
  nameS :: String,
  lawsS :: [LawS],
  gcsS  :: [S],
  definitionsS :: [S]
 } deriving (Eq, Show)

-------------------------------------------------------------------------------

