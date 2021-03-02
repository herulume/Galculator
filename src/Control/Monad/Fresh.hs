
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, 
    FlexibleInstances, UndecidableInstances, OverlappingInstances #-}
{-# OPTIONS_GHC -Wall #-}

-------------------------------------------------------------------------------

{- |
Module      :  Control.Monad.Fresh
Description :  Monad which provides an infinite set of values.
Copyright   :  (c) Paulo Silva
License     :  LGPL

Maintainer  :  paufil@di.uminho.pt
Stability   :  experimental
Portability :  portable

-}

-------------------------------------------------------------------------------

module Control.Monad.Fresh (
  MonadFresh,
  getFresh,
  FreshT(..),
  evalFreshT,
  getFreshLift
 ) where

-------------------------------------------------------------------------------

import Control.Monad
import Control.Monad.Trans
import Data.Stream

-------------------------------------------------------------------------------

class (Monad m, Stream s v) => MonadFresh s v m | m -> s where
  getFresh :: m v

-------------------------------------------------------------------------------

newtype FreshT s m a = FreshT {runFreshT :: s -> m (s, a)}

-------------------------------------------------------------------------------

instance Monad m => Functor (FreshT s m) where
  fmap f (FreshT t) = FreshT $ \s -> do
    (s', v') <- t s
    return (s',f v')

-------------------------------------------------------------------------------

instance Monad m => Monad (FreshT s m) where
  return v = FreshT $ \s -> return (s,v)
  v >>= f = FreshT $ \s -> do 
     (s',v') <- runFreshT v s
     runFreshT (f v') s'

-------------------------------------------------------------------------------

instance (Stream s v, Monad m) => MonadFresh s v (FreshT s m) where
  getFresh = FreshT $ \s -> return (tailStr s, headStr s)

-------------------------------------------------------------------------------

instance MonadTrans (FreshT s) where
  lift m = FreshT $ \s -> do
    x <- m
    return (s,x)

-------------------------------------------------------------------------------

instance MonadIO m => MonadIO (FreshT s m) where
  liftIO = lift . liftIO

-------------------------------------------------------------------------------

instance (Monad (t m), MonadTrans t, MonadFresh s v m) => MonadFresh s v (t m) where
  getFresh = lift getFresh

-------------------------------------------------------------------------------

evalFreshT :: Monad m => FreshT s m a -> s -> m a
evalFreshT m s = do
  ~(_,a) <- runFreshT m s
  return a

-------------------------------------------------------------------------------

getFreshLift :: MonadFresh s v m => (v -> a) -> m a
getFreshLift f = liftM f getFresh
-------------------------------------------------------------------------------
