
{-# OPTIONS_GHC -Wall #-}

-------------------------------------------------------------------------------

{- |
Module      :  Control.MonadPosition
Description :  
Copyright   :  (c) Paulo Silva
License     :  LGPL

Maintainer  :  paufil@di.uminho.pt
Stability   :  experimental
Portability :  portable
<description of the module>

-}

-------------------------------------------------------------------------------

module Control.MonadPosition (
  MonadPosition,
  branchLeft,
  branchRight,
  getPosition
 ) where
-------------------------------------------------------------------------------

type Pos = [Int]

class MonadPosition m where
  branchLeft :: m ()
  branchRight :: m ()
  getPosition :: m Pos

-------------------------------------------------------------------------------

