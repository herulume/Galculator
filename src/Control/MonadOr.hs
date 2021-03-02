
{-# OPTIONS_GHC -Wall #-}

-------------------------------------------------------------------------------

{- |
Module      :  Control.MonadOr
Description :  MonadOr definition.
Copyright   :  (c) Paulo Silva
License     :  LGPL

Maintainer  :  paufil@di.uminho.pt
Stability   :  experimental
Portability :  portable

Monad definition satisfying the Monoid and Left Catch laws as refered in
<http://www.haskell.org/haskellwiki/MonadPlus_reform_proposal>

-}

-------------------------------------------------------------------------------

module Control.MonadOr (
  MonadOr,
  mzero,
  morelse
 ) where

import Control.Monad.Error
import Control.Monad.State

-------------------------------------------------------------------------------

class MonadPlus m => MonadOr m where
  morelse :: m a -> m a -> m a

-------------------------------------------------------------------------------

instance MonadOr [] where
  morelse [] b = b
  morelse a _ = a

-------------------------------------------------------------------------------

-- This instance is equal to mplus for maybe
instance MonadOr Maybe where
  morelse = mplus

-------------------------------------------------------------------------------

instance (Monad m, Error e) => MonadOr (ErrorT e m) where
  morelse = mplus

-------------------------------------------------------------------------------

instance MonadOr m => MonadOr (StateT s m) where
    m `morelse` n = StateT $ \s -> runStateT m s `morelse` runStateT n s

-------------------------------------------------------------------------------
