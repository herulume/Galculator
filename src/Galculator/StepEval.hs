
{-# OPTIONS_GHC -Wall #-}

-------------------------------------------------------------------------------
 
{- |
Module      :  Galculator.StepEval
Description :  
Copyright   :  (c) Paulo Silva
License     :  LGPL
 
Maintainer  :  paufil@di.uminho.pt
Stability   :  experimental
Portability :  portable
 
-}

-------------------------------------------------------------------------------

module Galculator.StepEval (
  stepEval
 ) where

import Control.GalcError
import Control.Monad.Error
import Control.Monad.Fresh
import Data.Existential
import Galculator.Evaluate
import Galculator.Rule
import Galculator.State
import Galculator.Proof
import Language.Law.Syntax
import Language.R.Equality
import Language.R.Match
import qualified Language.R.Refresh as RR
import Language.R.SafeCast
import Language.R.Syntax
import Language.R.SyntaxADT
import qualified Language.R.TypeInference as RI
import qualified Language.R.Verify as RV
import Language.Step.Syntax
import Language.Type.Constraint
import Language.Type.Syntax
import Language.Type.Unification

-------------------------------------------------------------------------------

eitherId :: Either a a -> a
eitherId = either id id

-------------------------------------------------------------------------------

eitherFunct :: (a -> c) -> (b -> d) -> Either a b -> Either c d
eitherFunct f g = either (Left . f) (Right . g)

-------------------------------------------------------------------------------

compileR :: S -> GalcStateT RType
compileR s = do
  env <- getEnvironment
  RI.infer =<< RR.refresh =<< RV.verify env s  

-------------------------------------------------------------------------------
stepEval :: Step -> GalcStateT ()
-------------------------------------------------------------------------------
stepEval (Comb comb) = 
 inMode [ProofMode, IndirectProofMode] $ updateProof $ \prf -> do
  rl <- evalCombinator comb
  Exists t r <- getCurExpr
  return $ maybe prf 
                 (\(expr, proof) -> 
    prf {curExpr = eitherFunct (const (Exists t expr)) (const (Exists t expr)) (curExpr prf),
         curProof = addStep (RewriteStep (eitherId (curExpr prf)) proof) (curProof prf)}) (rewrite rl t r)
-------------------------------------------------------------------------------
stepEval (Indirect ord'') = inMode [ProofMode] $ (updateProof $ \prf -> do
  let cexpr = eitherId . curExpr $ prf
  ord' <- either compileR compileR ord''
  let ord = either (const (Left ord')) (const (Right ord')) ord''
  nexpr <- either (auxLeft cexpr)  (auxRight cexpr) ord
  return $ prf { 
    curExpr = eitherFunct (const nexpr) (const nexpr) (curExpr prf), 
    curProof = addStep (IndirectProof cexpr ord []) (curProof prf) } ) 
  >> enterIndirectProofMode
  where
    auxLeft :: RType -> RType -> GalcStateT RType
    auxLeft (Exists te e) (Exists to o) = do
      t1 <- getFreshLift (TVar . ('t':)); t2 <- getFreshLift (TVar . ('t':))
      constr <- unify [to :=: Ord t2, te :=: Fun t2 t1]
      Hide t1' <- typeRewrite constr t1
      Hide t2' <- typeRewrite constr t2
      o' <- rCast constr (Ord t2') o
      e' <- rCast constr (Fun t2' t1') e
      return $ Exists (Rel t2' t1') (COMP t2' (ORD o') (FUN e'))
    auxRight :: RType -> RType -> GalcStateT RType
    auxRight (Exists te e) (Exists to o) = do
      t1 <- getFreshLift (TVar . ('t':)); t2 <- getFreshLift (TVar . ('t':))
      constr <- unify [to :=: Ord t2, te :=: Fun t2 t1]
      Hide t1' <- typeRewrite constr t1
      Hide t2' <- typeRewrite constr t2
      o' <- rCast constr (Ord t2') o
      e' <- rCast constr (Fun t2' t1') e
      return $ Exists (Rel t1' t2') (COMP t2' (CONV (FUN e')) (ORD o'))
-------------------------------------------------------------------------------
stepEval IndirectEnd = inMode [IndirectProofMode] $ (updateProof $ \prf -> do
  let cexpr = eitherId . curExpr $ prf
  ord <- getOrd . curProof $ prf 
  nexpr <- either (auxLeft cexpr) (auxRight cexpr) ord
  return $ prf {curExpr = eitherFunct (const nexpr) (const nexpr) (curExpr prf),
           curProof = addStep QED (curProof prf)}) >> enterProofMode
  where
    auxLeft :: RType -> RType -> GalcStateT RType
    auxLeft (Exists te e) (Exists to o) = do
      t1 <- getFreshLift (TVar . ('t':)); t2 <- getFreshLift (TVar . ('t':))
      constr <- unify [to :=: Ord t2, te :=: Rel t2 t1]
      Hide t1' <- typeRewrite constr t1
      Hide t2' <- typeRewrite constr t2
      o' <- rCast constr (Ord t2') o
      e' <- rCast constr (Rel t2' t1') e
      r <- getFreshLift (Var . ('r':))
      rConstr <- rMatch (COMP t2' (ORD o') (FUN r)) e'
      let r' = rSubst rConstr (Fun t2' t1') r
      return $ Exists (Fun t2' t1') r'
    auxRight :: RType -> RType -> GalcStateT RType
    auxRight (Exists te e) (Exists to o) = do
      t1 <- getFreshLift (TVar . ('t':)); t2 <- getFreshLift (TVar . ('t':))
      constr <- unify [to :=: Ord t2, te :=: Rel t1 t2]
      Hide t1' <- typeRewrite constr t1
      Hide t2' <- typeRewrite constr t2
      o' <- rCast constr (Ord t2') o
      e' <- rCast constr (Rel t1' t2') e
      r <- getFreshLift (Var . ('r':))
      rConstr <- rMatch (COMP t2' (CONV (FUN r)) (ORD o')) e'
      let r' = rSubst rConstr (Fun t2' t1') r
      return $ Exists (Fun t2' t1') r' 
-------------------------------------------------------------------------------
stepEval LeftP = inMode [SetProofMode] $ (updateProof $ \prf -> 
  return $ prf {curExpr = Left $ getLeft (expression prf)}) >> enterProofMode
-------------------------------------------------------------------------------
stepEval Qed = inMode [ProofMode] $ (updateProof $ \prf -> 
     either (aux getRight prf) (aux getLeft prf) (curExpr prf)) >> 
    enterFinalProofMode
  where
   aux f p expr = 
     if req' expr (f (expression p)) 
     then return $ p {curProof = addStep QED (curProof p)}
     else throwError $ QedError (show expr) (show (f (expression p)))
   req' (Exists _ r) (Exists _ r') = req r r' 
-------------------------------------------------------------------------------
stepEval RightP = inMode [SetProofMode] $ (updateProof $ \prf -> 
  return $ prf {curExpr = Right $ getRight (expression prf)}) >> enterProofMode
-------------------------------------------------------------------------------
stepEval (SeqC s1 s2) = 
  stepEval s1 >> stepEval s2
-------------------------------------------------------------------------------