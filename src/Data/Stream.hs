
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances,
    TypeSynonymInstances, OverlappingInstances #-}
{-# OPTIONS_GHC -Wall #-}

-------------------------------------------------------------------------------

{- |
Module      :  Data.Stream
Description :  Streams of values.
Copyright   :  (c) Paulo Silva
License     :  LGPL

Maintainer  :  paufil@di.uminho.pt
Stability   :  experimental
Portability :  portable

-}

-------------------------------------------------------------------------------

module Data.Stream where

-------------------------------------------------------------------------------

class Stream a v | a -> v where
  headStr :: a -> v
  tailStr :: a -> a

-------------------------------------------------------------------------------

instance Stream [a] a where
  headStr = head
  tailStr = tail 

-------------------------------------------------------------------------------

instance Stream Int Int where
  headStr n = n
  tailStr n = succ n

-------------------------------------------------------------------------------

instance Stream [String] String where
  headStr = head
  tailStr = tail

-------------------------------------------------------------------------------

