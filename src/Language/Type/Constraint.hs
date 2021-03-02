
{-# LANGUAGE GADTs, PatternSignatures, FlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}
 
-------------------------------------------------------------------------------

{- |
Module      :  Language.Type.Constraint
Description :  Type equation constraints for type representation.
Copyright   :  (c) Paulo Silva
License     :  LGPL
  
Maintainer  :  paufil@di.uminho.pt
Stability   :  experimental
Portability :  portable
 
-}

-------------------------------------------------------------------------------
  
module Language.Type.Constraint  (
  Constraint(..),
  constraint2Rule,
  typeRewrite,
  typeRewriteE
 ) where

import Control.GalcError
import Control.Monad.Error
import Control.MonadOr
import Language.Type.Equality
import Language.Type.Rewrite
import Language.Type.Syntax

-------------------------------------------------------------------------------

data Constraint where 
  (:=:) :: Type a -> Type b -> Constraint

instance Show Constraint where
  show (t1 :=: t2) = show t1 ++ " = " ++ show t2
  
instance Eq Constraint where
  (a :=: b) == (a' :=: b') = beq a a' && beq b b'

-------------------------------------------------------------------------------

constraint2Rule :: Constraint -> Rule
constraint2Rule (t1 :=: t2) t1' = 
  if beq t1 t1' then return $ View t2 else mzero

-------------------------------------------------------------------------------

typeRewrite :: MonadOr m => [Constraint] -> Type t -> m TypeBox
typeRewrite constr t = do
  let rules::[Rule] = map constraint2Rule constr
  return . view2Box =<< everywhere (try (seqRules rules)) t

-------------------------------------------------------------------------------

typeRewriteE :: MonadError GalcError m => [Constraint] -> Type t -> m TypeBox
typeRewriteE constr t = 
  maybe2error (RewriteError (show t)) . typeRewrite constr $ t

-------------------------------------------------------------------------------
