
{-# LANGUAGE GADTs, FlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}
 
-------------------------------------------------------------------------------

{- |
Module      :  Language.R.TypeInference
Description :  Type inference for the expression representation.
Copyright   :  (c) Paulo Silva
License     :  LGPL
  
Maintainer  :  paufil@di.uminho.pt
Stability   :  experimental
Portability :  portable
 
-}

-------------------------------------------------------------------------------

module Language.R.TypeInference (
   infer,
   inferBin,
   catchInfer,
   InfEnv
 ) where

import Control.GalcError
import Control.Monad.Error
import Control.Monad.Fresh
import Control.Monad.State
import Data.Env
import Data.Existential
import Data.List
import qualified Data.Map as Map
import Language.R.SafeCast
import Language.R.Syntax
import Language.R.SyntaxADT
import Language.Type.Constraint
import Language.Type.Syntax
import Language.Type.Unification

-------------------------------------------------------------------------------

infer :: (MonadError GalcError m, MonadFresh [String] String m) --, MonadOr m)  
      => S -> m RType
infer s = evalStateT (infer' s) emptyEnv

-------------------------------------------------------------------------------

type InfEnv = Env TypeBox

-------------------------------------------------------------------------------

catchInfer :: (Show s, MonadError GalcError m) => s -> m a -> m a
catchInfer a f = f `catchError` (\e -> throwError $ InferenceError e (show a))

-------------------------------------------------------------------------------

addVariableEnv :: MonadState InfEnv m => String -> TypeBox -> m ()
addVariableEnv s t = modify (addEnv s t)

-------------------------------------------------------------------------------

updateEnvM :: (MonadError GalcError m, MonadState InfEnv m) 
          => [Constraint] -> InfEnv -> m ()
updateEnvM constr = 
  put . fmap ( \(Hide t) -> maybe (Hide t) id (typeRewrite constr t) )

-------------------------------------------------------------------------------

getUnifications :: [InfEnv] -> [Constraint]
getUnifications = aux . concatMap toList
  where 
    aux :: [(String,TypeBox)] -> [Constraint]
    aux [] = []
    aux ((n, Hide t):xs) = 
      (map (\(_, Hide t') -> t :=: t') . filter ((==) n . fst) $ xs) ++ aux xs

-------------------------------------------------------------------------------

getFreshT :: MonadFresh [String] String m => m (Type Var)
getFreshT = getFreshLift (TVar . ('t':))

-------------------------------------------------------------------------------
infer' :: (
           MonadFresh [String] String m,
           MonadError GalcError m, 
           MonadState InfEnv m
           ) 
       => S -> m RType
-------------------------------------------------------------------------------
infer' (RefS _ _) = throwError $ ImpossibleError
-------------------------------------------------------------------------------
infer' (RefExtS _ d) = return d
-------------------------------------------------------------------------------
infer' (BotS _) = do
  t1 <- getFreshT
  t2 <- getFreshT
  return $ Exists (Rel t1 t2) BOT
-------------------------------------------------------------------------------
infer' (TopS _) = do
  t1 <- getFreshT
  t2 <- getFreshT
  return $ Exists (Rel t1 t2) TOP
-------------------------------------------------------------------------------
infer' a@(NegS _ ra) = catchInfer a $ do
  (Exists tr r, envR) <- inferUni ra
  t1 <- getFreshT; t2 <- getFreshT
  constr <- unify [tr :=: Rel t1 t2]
  Hide t1'' <- typeRewriteE constr t1
  Hide t2'' <- typeRewriteE constr t2
  r' <- rCast constr (Rel t1'' t2'') r
  updateEnvM constr envR
  return $ Exists (Rel t1'' t2'') (NEG r')
-------------------------------------------------------------------------------
infer' a@(MeetS _ r1a r2a) = catchInfer a $ do
  (Exists tr1 r1, Exists tr2 r2, unif, envR) <- inferBin r1a r2a
  t1 <- getFreshT; t2 <- getFreshT; t1' <- getFreshT; t2' <- getFreshT
  constr <- unify $ 
    [tr1 :=: Rel t1 t2, tr2 :=: Rel t1' t2', t1 :=: t1', t2 :=: t2'] ++ unif
  Hide t1'' <- typeRewriteE constr t1
  Hide t2'' <- typeRewriteE constr t2
  r1' <- rCast constr (Rel t1'' t2'') r1
  r2' <- rCast constr (Rel t1'' t2'') r2
  updateEnvM constr envR
  return $ Exists (Rel t1'' t2'') (MEET r1' r2')
-------------------------------------------------------------------------------
infer' a@(JoinS _ r1a r2a) = catchInfer a $ do
  (Exists tr1 r1, Exists tr2 r2, unif, envR) <- inferBin r1a r2a
  t1 <- getFreshT; t2 <- getFreshT; t1' <- getFreshT; t2' <- getFreshT
  constr <- unify $ 
    [tr1 :=: Rel t1 t2, tr2 :=: Rel t1' t2', t1 :=: t1', t2 :=: t2'] ++ unif
  Hide t1'' <- typeRewriteE constr t1
  Hide t2'' <- typeRewriteE constr t2
  r1' <- rCast constr (Rel t1'' t2'') r1
  r2' <- rCast constr (Rel t1'' t2'') r2
  updateEnvM constr envR
  return $ Exists (Rel t1'' t2'') (JOIN r1' r2')
-------------------------------------------------------------------------------
infer' (IdS _) = do
  t <- getFreshT
  return $ Exists (Rel t t) ID
-------------------------------------------------------------------------------
infer' a@(ConvS _ ra) = catchInfer a $ do
  (Exists tr r, envR) <- inferUni ra
  t1 <- getFreshT; t2 <- getFreshT
  constr <- unify [tr :=: Rel t1 t2]
  Hide t1' <- typeRewriteE constr t1
  Hide t2' <- typeRewriteE constr t2
  r' <- rCast constr (Rel t1' t2') r
  updateEnvM constr envR
  return $ Exists (Rel t2' t1') (CONV r')
-------------------------------------------------------------------------------
infer' a@(CompS _ r1a r2a) = catchInfer a $ do
  (Exists tr1 r1, Exists tr2 r2, unif, envR) <- inferBin r1a r2a
  t1 <- getFreshT; t2 <- getFreshT; t2' <- getFreshT; t3 <- getFreshT
  constr <-  unify $ 
    [tr1 :=: Rel t3 t2, tr2 :=: Rel t2' t1, t2 :=: t2'] ++ unif
  Hide t1'' <- typeRewriteE constr t1
  Hide t2'' <- typeRewriteE constr t2
  Hide t3'' <- typeRewriteE constr t3
  r1' <- rCast constr (Rel t3'' t2'') r1
  r2' <- rCast constr (Rel t2'' t1'') r2
  updateEnvM constr envR
  return $ Exists (Rel t3'' t1'') (COMP t2'' r1' r2')
-------------------------------------------------------------------------------
infer' a@(SplitS _ r1a r2a) = catchInfer a $ do
  (Exists tr1 r1, Exists tr2 r2, unif, envR) <- inferBin r1a r2a
  t1 <- getFreshT; t2 <- getFreshT; t3 <- getFreshT; t1' <- getFreshT
  constr <- unify $ 
    [tr1 :=: Rel t2 t1, tr2 :=: Rel t3 t1', t1 :=: t1'] ++ unif
  Hide t1''  <- typeRewriteE constr t1
  Hide t2''  <- typeRewriteE constr t2
  Hide t3''  <- typeRewriteE constr t3
  r1' <- rCast constr (Rel t2'' t1'') r1
  r2' <- rCast constr (Rel t3'' t1'') r2
  updateEnvM constr envR
  return $ Exists (Rel (Prod t2'' t3'') t1'') (SPLIT r1' r2')
-------------------------------------------------------------------------------
infer' a@(OrdS _ ra) = catchInfer a $ do
  (Exists tr r, envR) <- inferUni ra
  t <- getFreshT
  constr <- unify [tr :=: Ord t]
  Hide t'' <- typeRewriteE constr t
  r' <- rCast constr (Ord t'') r
  updateEnvM constr envR
  return $ Exists (Rel t'' t'') (ORD r')
-------------------------------------------------------------------------------
infer' a@(FunS _ ra) = catchInfer a $ do
  (Exists tr r, envR) <- inferUni ra
  t1 <- getFreshT; t2 <- getFreshT
  constr <- unify [tr :=: Fun t2 t1]
  Hide t1'' <- typeRewriteE constr t1
  Hide t2'' <- typeRewriteE constr t2
  r' <- rCast constr (Fun t2'' t1'') r
  updateEnvM constr envR
  return $ Exists (Rel t2'' t1'') (FUN r')
-------------------------------------------------------------------------------
infer' a@(LeftsecS _ r1a r2a) = catchInfer a $ do
  (Exists tr1 r1, Exists tr2 r2, unif, envR) <- inferBin r1a r2a
  t1 <- getFreshT; t2 <- getFreshT; t3 <- getFreshT
  constr <- unify $ [tr1 :=: Fun t1 (Prod t2 t3), t2 :=: tr2] ++ unif
  Hide t1'' <- typeRewriteE constr t1
  Hide t2'' <- typeRewriteE constr t2
  Hide t3'' <- typeRewriteE constr t3
  r2' <- rCast constr t2'' r2
  r1' <- rCast constr (Fun t1'' (Prod t2'' t3'')) r1
  updateEnvM constr envR
  return $ Exists (Fun t1'' t3'') (LEFTSEC t2'' r1' r2')
-------------------------------------------------------------------------------
infer' a@(RightsecS _ r1a r2a) = catchInfer a $ do
  (Exists tr1 r1, Exists tr2 r2, unif, envR) <- inferBin r1a r2a
  t1 <- getFreshT; t2 <- getFreshT; t3 <- getFreshT
  constr <- unify $ [tr1 :=: Fun t1 (Prod t2 t3), t3 :=: tr2] ++ unif
  Hide t1'' <- typeRewriteE constr t1
  Hide t2'' <- typeRewriteE constr t2
  Hide t3'' <- typeRewriteE constr t3
  r2' <- rCast constr t3'' r2
  r1' <- rCast constr (Fun t1'' (Prod t2'' t3'')) r1
  updateEnvM constr envR
  return $ Exists (Fun t1'' t2'') (RIGHTSEC t3'' r1' r2')
-------------------------------------------------------------------------------
infer' a@(ApplyS _ r1a r2a) = catchInfer a $ do
  (Exists tr1 r1, Exists tr2 r2, unif, envR) <- inferBin r1a r2a
  t1 <- getFreshT; t2 <- getFreshT
  constr <- unify $ [tr1 :=: Fun t1 t2, t2 :=: tr2] ++ unif
  Hide t2'' <- typeRewriteE constr t2
  Hide t1'' <- typeRewriteE constr t1
  r1' <- rCast constr (Fun t1'' t2'') r1
  r2' <- rCast constr t2'' r2
  updateEnvM constr envR
  return $ Exists t1'' (APPLY t2'' r1' r2')
-------------------------------------------------------------------------------
infer' (DefS _ n (Hide t)) = return $ Exists t (DEF n t)
-------------------------------------------------------------------------------
infer' (VarS _ n) = do
  t <- getFreshT
  addVariableEnv n (Hide t)
  return $ Exists t (Var n)
-------------------------------------------------------------------------------
infer' a@(ProdS _ r1a r2a) = catchInfer a $ do
  (Exists tr1 r1, Exists tr2 r2, unif, envR) <- inferBin r1a r2a
  t1 <- getFreshT; t2 <- getFreshT; t3 <- getFreshT; t4 <- getFreshT
  constr <- unify $ [tr1 :=: Rel t3 t1, tr2 :=: Rel t4 t2] ++ unif
  Hide t1'' <- typeRewriteE constr t1
  Hide t2'' <- typeRewriteE constr t2
  Hide t3'' <- typeRewriteE constr t3
  Hide t4'' <- typeRewriteE constr t4
  r1' <- rCast constr (Rel t3'' t1'') r1
  r2' <- rCast constr (Rel t4'' t2'') r2
  updateEnvM constr envR
  return $ Exists (Rel (Prod t3'' t4'') (Prod t1'' t2'')) (PROD r1' r2')
-------------------------------------------------------------------------------
infer' a@(EitherS _ r1a r2a) = catchInfer a $ do
  (Exists tr1 r1, Exists tr2 r2, unif, envR) <- inferBin r1a r2a
  t1 <- getFreshT; t2 <- getFreshT; t3 <- getFreshT; t4 <- getFreshT
  constr <- unify $ [tr1 :=: Rel t3 t1, tr2 :=: Rel t4 t2] ++ unif
  Hide t1'' <- typeRewriteE constr t1
  Hide t2'' <- typeRewriteE constr t2
  Hide t3'' <- typeRewriteE constr t3
  Hide t4'' <- typeRewriteE constr t4
  r1' <- rCast constr (Rel t3'' t1'') r1
  r2' <- rCast constr (Rel t4'' t2'') r2
  updateEnvM constr envR
  return $ Exists (Rel (Either t3'' t4'') (Either t1'' t2'')) (EITHER r1' r2')
-------------------------------------------------------------------------------
infer' a@(MaybeS _ ra) = catchInfer a $ do
  (Exists tr r, envR) <- inferUni ra 
  t1 <- getFreshT; t2 <- getFreshT
  constr <- unify [tr :=: Rel t2 t1]
  Hide t1'' <- typeRewriteE constr t1
  Hide t2'' <- typeRewriteE constr t2
  r1' <- rCast constr (Rel t2'' t1'') r
  updateEnvM constr envR
  return $ Exists (Rel (Maybe t2'') (Maybe t1'')) (MAYBE r1')
-------------------------------------------------------------------------------
infer' a@(ListS _ ra) = catchInfer a $ do
  (Exists tr r, envR) <- inferUni ra 
  t1 <- getFreshT; t2 <- getFreshT
  constr <- unify [tr :=: Rel t2 t1]
  Hide t1'' <- typeRewriteE constr t1
  Hide t2'' <- typeRewriteE constr t2
  r1' <- rCast constr (Rel t2'' t1'') r
  updateEnvM constr envR
  return $ Exists (Rel (List t2'') (List t1'')) (LIST r1')
-------------------------------------------------------------------------------
infer' a@(SetS _ ra) = catchInfer a $ do
  (Exists tr r, envR) <- inferUni ra 
  t1 <- getFreshT; t2 <- getFreshT
  constr <- unify [tr :=: Rel t2 t1]
  Hide t1'' <- typeRewriteE constr t1
  Hide t2'' <- typeRewriteE constr t2
  r1' <- rCast constr (Rel t2'' t1'') r
  updateEnvM constr envR
  return $ Exists (Rel (Set t2'') (Set t1'')) (SET r1')
-------------------------------------------------------------------------------
infer' a@(MapS _ ra) = catchInfer a $ do
  (Exists tr r, envR) <- inferUni ra 
  t1 <- getFreshT; t2 <- getFreshT; t3 <- getFreshT
  constr <- unify [tr :=: Rel t2 t1]
  Hide t1'' <- typeRewriteE constr t1
  Hide t2'' <- typeRewriteE constr t2
  r1' <- rCast constr (Rel t2'' t1'') r
  updateEnvM constr envR
  return $ Exists (Rel (Map t3 t2'') (Map t3 t1'')) (MAP r1')
-------------------------------------------------------------------------------
infer' a@(ReynoldsS _ r1a r2a) = catchInfer a $ do
  (Exists tr1 r1, Exists tr2 r2, unif, envR) <- inferBin r1a r2a
  t1 <- getFreshT; t2 <- getFreshT; t3 <- getFreshT; t4 <- getFreshT
  constr <- unify $ [tr1 :=: Rel t2 t1, tr2 :=: Rel t4 t3] ++ unif
  Hide t1' <- typeRewriteE constr t1
  Hide t2' <- typeRewriteE constr t2
  Hide t3' <- typeRewriteE constr t3
  Hide t4' <- typeRewriteE constr t4
  r1' <- rCast constr (Rel t2' t1') r1
  r2' <- rCast constr (Rel t4' t3') r2
  updateEnvM constr envR
  return $ Exists (Rel (Fun t2' t4') (Fun t1' t3')) (REYNOLDS r1' r2')
-------------------------------------------------------------------------------
infer' (FIdS _) = do
  t <- getFreshT
  return $ Exists (Fun t t) FId
-------------------------------------------------------------------------------
infer' a@(FCompS _ r1a r2a) = catchInfer a $ do
  (Exists tr1 r1, Exists tr2 r2, unif, envR) <- inferBin r1a r2a
  t1 <- getFreshT; t2 <- getFreshT; t3 <- getFreshT; t2' <- getFreshT
  constr <- unify $ 
    [tr1 :=: Fun t1 t2, tr2 :=: Fun t2' t3, t2 :=: t2'] ++ unif
  Hide t1'' <- typeRewriteE constr t1
  Hide t2'' <- typeRewriteE constr t2
  Hide t3'' <- typeRewriteE constr t3
  r1' <- rCast constr (Fun t1'' t2'') r1
  r2' <- rCast constr (Fun t2'' t3'') r2
  updateEnvM constr envR
  return $ Exists (Fun t1'' t3'') (FComp t2'' r1' r2')
-------------------------------------------------------------------------------
infer' (OIdS _) = do
  t <- getFreshT
  return $ Exists (Ord t) OId
-------------------------------------------------------------------------------
infer' a@(OCompS _ r1a r2a) = catchInfer a $ do
  (Exists tr1 r1, Exists tr2 r2, unif, envR) <- inferBin r1a r2a
  t <- getFreshT; t' <- getFreshT
  constr <- unify $ [tr1 :=: Ord t, tr2 :=: Ord t', t :=: t'] ++ unif
  Hide t'' <- typeRewriteE constr t
  r1' <- rCast constr (Ord t'') r1
  r2' <- rCast constr (Ord t'') r2
  updateEnvM constr envR
  return $ Exists (Ord t'') (OComp r1' r2')
-------------------------------------------------------------------------------
infer' a@(OConvS _ ra) = catchInfer a $ do
  (Exists tr r, envR) <- inferUni ra
  t <- getFreshT
  constr <- unify [tr :=: Ord t]
  Hide t'' <- typeRewriteE constr t
  r' <- rCast constr (Ord t'') r
  updateEnvM constr envR
  return $ Exists (Ord t'') (OConv r')
-------------------------------------------------------------------------------
infer' a@(OProdS _ ra) = catchInfer a $ do
  (Exists tr r, envR) <- inferUni ra
  t <- getFreshT
  constr <- unify [tr :=: Ord t]
  Hide t'' <- typeRewriteE constr t
  r' <- rCast constr (Ord t'') r
  updateEnvM constr envR
  return $ Exists (Ord (Prod t'' t'')) (OProd r')
-------------------------------------------------------------------------------
infer' a@(OJoinS _ ra) = catchInfer a $ do
  (Exists tr r, envR) <- inferUni ra
  t <- getFreshT
  constr <- unify [tr :=: Ord t]
  Hide t'' <- typeRewriteE constr t
  r' <- rCast constr (Ord t'') r
  updateEnvM constr envR
  return $ Exists (Fun t'' (Prod t'' t'')) (OJoin r')
-------------------------------------------------------------------------------
infer' a@(OMeetS _ ra) = catchInfer a $ do
  (Exists tr r, envR) <- inferUni ra
  t <- getFreshT
  constr <- unify [tr :=: Ord t]
  Hide t'' <- typeRewriteE constr t
  r' <- rCast constr (Ord t'') r
  updateEnvM constr envR
  return $ Exists (Fun t'' (Prod t'' t'')) (OMeet r')
-------------------------------------------------------------------------------
infer' a@(OMaxS _ ra) = catchInfer a $ do
  (Exists tr r, envR) <- inferUni ra
  t <- getFreshT
  constr <- unify [tr :=: Ord t]
  Hide t'' <- typeRewriteE constr t
  r' <- rCast constr (Ord t'') r
  updateEnvM constr envR
  return $ Exists t'' (OMax r')
-------------------------------------------------------------------------------
infer' a@(OMinS _ ra) = catchInfer a $ do
  (Exists tr r, envR) <- inferUni ra
  t <- getFreshT
  constr <- unify [tr :=: Ord t]
  Hide t'' <- typeRewriteE constr t
  r' <- rCast constr (Ord t'') r
  updateEnvM constr envR
  return $ Exists t'' (OMin r')
-------------------------------------------------------------------------------
infer' a@(GDefS _ n f1a f2a o1a o2a) = catchInfer a $ do
  (Exists tf1 f1, envF1) <- inferUni f1a
  (Exists tf2 f2, envF2) <- inferUni f2a
  (Exists to1 o1, envO1) <- inferUni o1a
  (Exists to2 o2, envO2) <- inferUni o2a
  t1   <- getFreshT; t2   <- getFreshT
  t1'  <- getFreshT; t2'  <- getFreshT
  t1'' <- getFreshT; t2'' <- getFreshT
  let envR = envF1 `Map.union` envF2 `Map.union` envO1 `Map.union` envO2
      unif = getUnifications [envF1,envF2,envO1,envO2]
  constr <- unify $ 
    [tf1 :=: Fun t1 t2, tf2 :=: Fun t2' t1', to1 :=: Ord t1'', to2 :=: Ord t2'',
     t1 :=: t1', t1' :=: t1'', t2 :=: t2', t2' :=: t2''] ++ unif
  Hide t1''' <- typeRewriteE constr t1
  Hide t2''' <- typeRewriteE constr t2
  f1' <- rCast constr (Fun t1''' t2''') f1
  f2' <- rCast constr (Fun t2''' t1''') f2
  o1' <- rCast constr (Ord t1''') o1
  o2' <- rCast constr (Ord t2''') o2
  updateEnvM constr envR
  return $ Exists (GC t1''' t2''') (GDef n f1' f2' o1' o2')
-------------------------------------------------------------------------------
infer' (GIdS _) = do
  t <- getFreshT
  return $ Exists (GC t t) GId
-------------------------------------------------------------------------------
infer' a@(GCompS _ r1a r2a) = catchInfer a $ do
  (Exists tr1 r1, Exists tr2 r2, unif, envR) <- inferBin r1a r2a
  t1 <- getFreshT; t2 <- getFreshT; t3 <- getFreshT; t2' <- getFreshT
  constr <- unify $
    [tr1 :=: GC t1 t2, tr2 :=: GC t2' t3, t2 :=: t2'] ++ unif
  Hide t1'' <- typeRewriteE constr t1
  Hide t2'' <- typeRewriteE constr t2
  Hide t3'' <- typeRewriteE constr t3
  r1' <- rCast constr (GC t1'' t2'') r1
  r2' <- rCast constr (GC t2'' t3'') r2
  updateEnvM constr envR
  return $ Exists (GC t1'' t3'') (GComp t2'' r1' r2')
-------------------------------------------------------------------------------
infer' a@(GConvS _ ra) = catchInfer a $ do
  (Exists tr r, envR) <- inferUni ra
  t1 <- getFreshT; t2 <- getFreshT
  constr <- unify [tr :=: GC t1 t2]
  Hide t1'' <- typeRewriteE constr t1
  Hide t2'' <- typeRewriteE constr t2
  r' <- rCast constr (GC t1'' t2'') r
  updateEnvM constr envR
  return $ Exists (GC t2'' t1'') (GConv r')
-------------------------------------------------------------------------------
infer' a@(GLAdjS _ ra) = catchInfer a $ do
  (Exists tr r, envR) <- inferUni ra
  t1 <- getFreshT; t2 <- getFreshT
  constr <- unify [tr :=: GC t1 t2]
  Hide t1'' <- typeRewriteE constr t1
  Hide t2'' <- typeRewriteE constr t2
  r' <- rCast constr (GC t1'' t2'') r
  updateEnvM constr envR
  return $ Exists (Fun t1'' t2'') (GLAdj r')
-------------------------------------------------------------------------------
infer' a@(GUAdjS _ ra) = catchInfer a $ do
  (Exists tr r, envR) <- inferUni ra
  t1 <- getFreshT; t2 <- getFreshT
  constr <- unify [tr :=: GC t1 t2]
  Hide t1'' <- typeRewriteE constr t1
  Hide t2'' <- typeRewriteE constr t2
  r' <- rCast constr (GC t1'' t2'') r
  updateEnvM constr envR
  return $ Exists (Fun t2'' t1'') (GUAdj r')
-------------------------------------------------------------------------------
infer' a@(GLOrdS _ ra) = catchInfer a $ do
  (Exists tr r, envR) <- inferUni ra
  t1 <- getFreshT; t2 <- getFreshT
  constr <- unify [tr :=: GC t1 t2]
  Hide t1'' <- typeRewriteE constr t1
  Hide t2'' <- typeRewriteE constr t2
  r' <- rCast constr (GC t1'' t2'') r
  updateEnvM constr envR
  return $ Exists (Ord t1'') (GLOrd t2'' r')
-------------------------------------------------------------------------------
infer' a@(GUOrdS _ ra) = catchInfer a $ do
  (Exists tr r, envR) <- inferUni ra
  t1 <- getFreshT; t2 <- getFreshT
  constr <- unify [tr :=: GC t1 t2]
  Hide t1'' <- typeRewriteE constr t1
  Hide t2'' <- typeRewriteE constr t2
  r' <- rCast constr (GC t1'' t2'') r
  updateEnvM constr envR
  return $ Exists (Ord t2'') (GUOrd t1'' r')
-------------------------------------------------------------------------------

inferUni :: (MonadError GalcError m, 
             MonadState InfEnv m, 
             MonadFresh [String] String m) 
         => S -> m (RType, InfEnv)
inferUni r = do 
  x <- infer' r
  envR <- get
  put emptyEnv
  return (x, envR)

-------------------------------------------------------------------------------

inferBin :: (MonadError GalcError m, 
             MonadState InfEnv m, 
             MonadFresh [String] String m)
         => S -> S -> m (RType, RType, [Constraint], InfEnv)
inferBin r1 r2 = do
  (x, envR1) <- inferUni r1
  (y, envR2) <- inferUni r2
  let envR = envR1 `Map.union` envR2 
      unif = getUnifications [envR1,envR2]
  return (x,y,unif,envR)

-------------------------------------------------------------------------------

