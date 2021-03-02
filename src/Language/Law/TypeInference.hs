
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}
 
-------------------------------------------------------------------------------

{- |
Module      :  Language.Law.TypeInference
Description :  Type inference for the law representation.
Copyright   :  (c) Paulo Silva
License     :  LGPL
  
Maintainer  :  paufil@di.uminho.pt
Stability   :  experimental
Portability :  portable
 
-}

-------------------------------------------------------------------------------

module Language.Law.TypeInference (
  infer
 ) where

import Control.GalcError
import Control.MonadOr
import Control.Monad.Error
import Control.Monad.Fresh
import Control.Monad.State
import Data.Existential
import Data.Env
import Language.Law.Syntax
import Language.Law.SyntaxADT
import Language.R.SafeCast
import qualified Language.R.TypeInference as R
import Language.Type.Constraint
import Language.Type.Unification

-------------------------------------------------------------------------------

infer :: (MonadOr m, MonadError GalcError m, MonadFresh [String] String m) 
      => LawS -> m Law
infer s = evalStateT (infer' s) emptyEnv

-------------------------------------------------------------------------------
infer' :: (MonadOr m, MonadError GalcError m, MonadState R.InfEnv m, MonadFresh [String] String m) 
       => LawS -> m Law
-------------------------------------------------------------------------------
infer' a@(EquivS _ ident r1a r2a) = R.catchInfer a $ do
  (Exists tr1 r1, Exists tr2 r2, unif, _) <- R.inferBin r1a r2a
  constr <- unify $ [tr1 :=: tr2] ++ unif
  Hide t1' <- typeRewrite constr tr1
  r1' <- rCast constr t1' r1
  r2' <- rCast constr t1' r2
  return $ EQUIV (Meta {name = ident, ruleType = Nothing}) t1' r1' r2'
-------------------------------------------------------------------------------
infer' a@(ImplS _ ident r1a r2a) = R.catchInfer a $ do
  (Exists tr1 r1, Exists tr2 r2, unif, _) <- R.inferBin r1a r2a
  constr <- unify $ [tr1 :=: tr2] ++ unif
  Hide t1' <- typeRewrite constr tr1
  r1' <- rCast constr t1' r1
  r2' <- rCast constr t1' r2
  return $ IMPL (Meta {name = ident, ruleType = Nothing}) t1' r1' r2'
-------------------------------------------------------------------------------


