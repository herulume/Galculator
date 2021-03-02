
{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wall #-}

-------------------------------------------------------------------------------

{- |
Module      :  Data.Existential
Description :  Existential type patterns.
Copyright   :  (c) Paulo Silva
License     :  LGPL

Maintainer  :  paufil@di.uminho.pt
Stability   :  experimental
Portability :  portable

-}

-------------------------------------------------------------------------------

module Data.Existential where

data Exists singleton term = forall t . Exists (singleton t) (term t)

data Covert t = forall x . Hide (t x)

