
{-# LANGUAGE GADTs, EmptyDataDecls #-}
{-# OPTIONS_GHC -Wall #-}

-------------------------------------------------------------------------------

{- |
Module      :  Language.Module.Syntax
Description :  Representation of the modules used by Galculator.
Copyright   :  (c) Paulo Silva
License     :  LGPL

Maintainer  :  paufil@di.uminho.pt
Stability   :  experimental
Portability :  portable

-}

-------------------------------------------------------------------------------

module Language.Module.Syntax (
  Module(name,laws,gcs,definitions,Module),
 ) where

import Data.Env
import Language.Law.Syntax hiding (name)
import Language.R.Syntax

-------------------------------------------------------------------------------

data Module   =  Module {
  name        :: String,
  laws        :: Env Law,
  gcs         :: Env RType,
  definitions :: Env RType
 } 

-------------------------------------------------------------------------------

instance Show Module where
 show m = Language.Module.Syntax.name m ++ show (laws m) ++ show (gcs m)

-------------------------------------------------------------------------------

