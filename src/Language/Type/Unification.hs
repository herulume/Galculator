
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}
 
-------------------------------------------------------------------------------

{- |
Module      :  Language.Type.Unification
Description :  Unification algorithm for the polymorphic type representation.
Copyright   :  (c) Paulo Silva
License     :  LGPL
  
Maintainer  :  paufil@di.uminho.pt
Stability   :  experimental
Portability :  portable
 
-}

-------------------------------------------------------------------------------
  
module Language.Type.Unification  (
 unify
 )   where

import Control.GalcError
import Control.Monad.Error
import Data.Existential
import Language.Type.Constraint
import Language.Type.Equality
import Language.Type.Syntax
import Language.Type.Utils
import Prelude hiding (all,fail)

-------------------------------------------------------------------------------

unify :: (MonadError GalcError m) 
      => [Constraint] -> m [Constraint]
unify [] = return []
unify ((s :=: t):c)
  | isTVar s && isTVar t && beq s t =              -- delete rule (variables)
      unify c
  | isBasicType s && isBasicType t && beq s t =    -- delete rule (base types)
      unify c
  | isTVar t && not (isTVar s) =                     -- orient rule
      unify $ (t :=: s):c
  | isConstructor s && isConstructor t = do          -- decompose rule
      cns <- decompose s t
      unify $ cns++c
  | isTVar s && not (occursIn s t) = do              -- eliminate rule
      c' <- substitution [s :=: t] c
      sbs <- unify c'
      Hide t' <- typeRewriteE sbs t
      return $ (s :=: t'):sbs
  | otherwise = 
      throwError $ UnificationError (show s, show t)  -- typing error

-------------------------------------------------------------------------------

substitution :: MonadError GalcError m 
             => [Constraint] -> [Constraint] -> m [Constraint]
substitution subst = mapM aux
  where aux (t1 :=: t2) = do
          Hide t1' <- typeRewriteE subst t1
          Hide t2' <- typeRewriteE subst t2
          return $ t1' :=: t2'

-------------------------------------------------------------------------------

decompose :: MonadError GalcError m => Type a -> Type b -> m [Constraint]
decompose (Prod a b) (Prod a' b') = return [a :=: a', b :=: b']
decompose (Either a b) (Either a' b') = return [a :=: a', b :=: b']
decompose (Maybe a) (Maybe a') = return [a :=: a']
decompose (List a) (List a') = return [a :=: a']
decompose (Set a) (Set a') = return [a :=: a']
decompose (Map a b) (Map a' b') = return [a :=: a', b :=: b']
decompose (Fun a b) (Fun a' b') = return [a :=: a', b :=: b']
decompose (Rel a b) (Rel a' b') = return [a :=: a', b :=: b']
decompose (Ord a) (Ord a') = return [a :=: a']
decompose (GC a b) (GC a' b') = return [a :=: a', b :=: b']
decompose t1 t2 = throwError $ DecomposeError (show t1) (show t2)

-------------------------------------------------------------------------------

