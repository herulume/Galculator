
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall #-}

-------------------------------------------------------------------------------

{- |
Module      :  Data.Equal
Description :  
Copyright   :  (c) Paulo Silva
License     :  LGPL

Maintainer  :  paufil@di.uminho.pt
Stability   :  experimental
Portability :  portable

<description of the module>
-}

-------------------------------------------------------------------------------

module Data.Equal where

data Equal a b where
  Eq :: Equal a a

-------------------------------------------------------------------------------

