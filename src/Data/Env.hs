
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}

-------------------------------------------------------------------------------

{- |
Module      :  Data.Env
Description :  Value environment.
Copyright   :  (c) Paulo Silva
License     :  LGPL

Maintainer  :  paufil@di.uminho.pt
Stability   :  experimental
Portability :  portable

-}

-------------------------------------------------------------------------------

module Data.Env (
  Env,
  emptyEnv,
  fromListEnv,
  lookupEnv,
  addEnv,
  clearEnv,
  toList,
  indexes,
  values,
  delete,
  joinEnv
 ) where

import Control.Monad.Error
import Data.Map(Map)
import qualified Data.Map as Map

-------------------------------------------------------------------------------

type Env t = Map String t

-------------------------------------------------------------------------------

emptyEnv :: Env t
emptyEnv = Map.empty

-------------------------------------------------------------------------------

fromListEnv :: [(String,t)] -> Env t
fromListEnv = Map.fromList

-------------------------------------------------------------------------------

lookupEnv :: MonadPlus m => String -> Env t -> m t
lookupEnv n env =  Map.lookup n env

-------------------------------------------------------------------------------

addEnv :: String -> t -> Env t -> Env t
addEnv k t = Map.insert k t

-------------------------------------------------------------------------------

clearEnv :: Env t -> Env t
clearEnv = const emptyEnv

-------------------------------------------------------------------------------

toList :: Env t -> [(String,t)]
toList = Map.toList

-------------------------------------------------------------------------------

indexes :: Env t -> [String]
indexes = Map.keys

-------------------------------------------------------------------------------

values :: Env t -> [t]
values = Map.elems

-------------------------------------------------------------------------------

delete :: String -> Env t -> Env t
delete = Map.delete

-------------------------------------------------------------------------------

joinEnv :: Env t -> Env t -> Env t
joinEnv = Map.union

-------------------------------------------------------------------------------

