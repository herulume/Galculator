
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}

-------------------------------------------------------------------------------

{- |
Module      :  Language.R.Refresh
Description :  Operations for refreshing the variable names.
Copyright   :  (c) Paulo Silva
License     :  LGPL

Maintainer  :  paufil@di.uminho.pt
Stability   :  experimental
Portability :  portable

-}

-------------------------------------------------------------------------------

module Language.R.Refresh (
  refresh,
  collect,
  replace,
  refreshVar,
  refreshType,
  St(..)
 ) where

import Control.Monad.Fresh
import Control.Monad.Reader
import Data.Existential
import Language.R.SyntaxADT
import Language.Type.Constraint
import Language.Type.Syntax
import Language.Type.Utils

-------------------------------------------------------------------------------

refresh :: MonadFresh [String] String m => S -> m S
refresh s = do
    let (rvar,tvar) = collect s
    newVars <- refreshVar rvar
    newTypes <- refreshType tvar
    return . runReader (replace s) $ St {rvars = newVars, tvars = newTypes}

-------------------------------------------------------------------------------

-- ============
data St = St {rvars :: [(String,String)], tvars :: [Constraint]}
-- ============

-------------------------------------------------------------------------------

refreshVar :: MonadFresh [String] String m => [String] -> m [(String,String)]
refreshVar = mapM (\x -> do v <- getFresh; return $ (x,'r':v))

-------------------------------------------------------------------------------

refreshType :: MonadFresh [String] String m => [String] -> m [Constraint]
refreshType = mapM (\x -> do v <- getFresh; return $ TVar x :=: TVar ('t':v))

-------------------------------------------------------------------------------

collect :: S -> ([String], [String])
collect (DefS _ _ t) = ([], map getTVarNameTB . collectTVarTB $ t)
collect (VarS _ n) = ([n], [])
collect (NegS _ s) = collect s
collect (MeetS _ s1 s2) = let
   (i1, tb1) = collect s1
   (i2, tb2) = collect s2
  in (i1++i2, tb1++tb2)
collect (JoinS _ s1 s2) = let
   (i1, tb1) = collect s1
   (i2, tb2) = collect s2
  in (i1++i2, tb1++tb2)
collect (ConvS _ s) = collect s
collect (CompS _ s1 s2) = let
   (i1, tb1) = collect s1
   (i2, tb2) = collect s2
  in (i1++i2, tb1++tb2)
collect (SplitS _ s1 s2) = let
   (i1, tb1) = collect s1
   (i2, tb2) = collect s2
  in (i1++i2, tb1++tb2)
collect (OrdS _ s) = collect s
collect (FunS _ s) = collect s
collect (LeftsecS _ s1 s2) = let
   (i1, tb1) = collect s1
   (i2, tb2) = collect s2
  in (i1++i2, tb1++tb2)
collect (RightsecS _ s1 s2) = let
   (i1, tb1) = collect s1
   (i2, tb2) = collect s2
  in (i1++i2, tb1++tb2)
collect (ApplyS _ s1 s2) = let
   (i1, tb1) = collect s1
   (i2, tb2) = collect s2
  in (i1++i2, tb1++tb2)
collect (ProdS _ s1 s2) = let
   (i1, tb1) = collect s1
   (i2, tb2) = collect s2
  in (i1++i2, tb1++tb2)
collect (EitherS _ s1 s2) = let
   (i1, tb1) = collect s1
   (i2, tb2) = collect s2
  in (i1++i2, tb1++tb2)
collect (MaybeS _ s) = collect s
collect (ListS _ s) = collect s
collect (SetS _ s) = collect s
collect (MapS _ s) = collect s
collect (ReynoldsS _ s1 s2) = let
   (i1, tb1) = collect s1
   (i2, tb2) = collect s2
  in (i1++i2, tb1++tb2)
collect (FCompS _ s1 s2) = let
   (i1, tb1) = collect s1
   (i2, tb2) = collect s2
  in (i1++i2, tb1++tb2)
collect (OCompS _ s1 s2) = let
   (i1, tb1) = collect s1
   (i2, tb2) = collect s2
  in (i1++i2, tb1++tb2)
collect (OConvS _ s) = collect s
collect (OProdS _ s) = collect s
collect (OJoinS _ s) = collect s
collect (OMeetS _ s) = collect s
collect (OMaxS _ s) = collect s
collect (OMinS _ s) = collect s
collect (GDefS _ _ f1 f2 o1 o2) = let
   (if1, tbf1) = collect f1
   (if2, tbf2) = collect f2
   (io1, tbo1) = collect o1
   (io2, tbo2) = collect o2
  in (if1 ++ if2 ++ io1 ++ io2, tbf1 ++ tbf2 ++ tbo1 ++ tbo2)
collect (GCompS _ s1 s2) = let
   (i1, tb1) = collect s1
   (i2, tb2) = collect s2
  in (i1++i2, tb1++tb2)
collect (GConvS _ s) = collect s
collect (GLAdjS _ s) = collect s
collect (GUAdjS _ s) = collect s
collect (GLOrdS _ s) = collect s
collect (GUOrdS _ s) = collect s
collect _ = ([],[])

-------------------------------------------------------------------------------

replace :: MonadReader St m => S -> m S
replace (DefS p n (Hide t)) = do
  env <- ask
  let Just t' = typeRewrite (tvars env) t
  return $ DefS p n t'
replace (VarS p n) = do
  env <- ask
  let Just n' = lookup n . rvars $ env
  return $ VarS p n'
replace (NegS p s) = do 
  s' <- replace s
  return $ NegS p s' 
replace (MeetS p s1 s2) = do
  s1' <- replace s1
  s2' <- replace s2
  return $ MeetS p s1' s2'
replace (JoinS p s1 s2) = do
  s1' <- replace s1
  s2' <- replace s2
  return $ JoinS p s1' s2'
replace (ConvS p s) = do 
  s' <- replace s
  return $  ConvS p s'
replace (CompS p s1 s2) = do
  s1' <- replace s1
  s2' <- replace s2
  return $ CompS p s1' s2'
replace (SplitS p s1 s2) = do
  s1' <- replace s1
  s2' <- replace s2
  return $ SplitS p s1' s2'
replace (OrdS p s) = do 
  s' <- replace s
  return $ OrdS p s'
replace (FunS p s) = do 
  s' <- replace s
  return $ FunS p s'
replace (LeftsecS p s1 s2) = do
  s1' <- replace s1
  s2' <- replace s2
  return $ LeftsecS p s1' s2'
replace (RightsecS p s1 s2) = do
  s1' <- replace s1
  s2' <- replace s2
  return $ RightsecS p s1' s2'
replace (ApplyS p s1 s2) = do
  s1' <- replace s1
  s2' <- replace s2
  return $ ApplyS p s1' s2'
replace (ProdS p s1 s2) = do
  s1' <- replace s1
  s2' <- replace s2
  return $ ProdS p s1' s2'
replace (EitherS p s1 s2) = do
  s1' <- replace s1
  s2' <- replace s2
  return $ EitherS p s1' s2'
replace (MaybeS p s) = do 
  s' <- replace s
  return $ MaybeS p s'
replace (ListS p s) = do 
  s' <- replace s
  return $ ListS p s'
replace (SetS p s) = do 
  s' <- replace s
  return $ SetS p s'
replace (MapS p s) = do 
  s' <- replace s
  return $ MapS p s'
replace (ReynoldsS p s1 s2) = do
  s1' <- replace s1
  s2' <- replace s2
  return $ ReynoldsS p s1' s2'
replace (FCompS p s1 s2) = do
  s1' <- replace s1
  s2' <- replace s2
  return $ FCompS p s1' s2'
replace (OCompS p s1 s2) = do
  s1' <- replace s1
  s2' <- replace s2
  return $ OCompS p s1' s2'
replace (OConvS p s) = do 
  s' <- replace s
  return $ OConvS p s'
replace (OProdS p s) = do 
  s' <- replace s
  return $ OProdS p s'
replace (OJoinS p s) = do 
  s' <- replace s
  return $ OJoinS p s'
replace (OMeetS p s) = do 
  s' <- replace s
  return $ OMeetS p s'
replace (OMaxS p s) = do 
  s' <- replace s
  return $ OMaxS p s'
replace (OMinS p s) = do 
  s' <- replace s
  return $ OMinS p s'
replace (GDefS p n f1 f2 o1 o2) = do
  f1' <- replace f1
  f2' <- replace f2
  o1' <- replace o1
  o2' <- replace o2
  return $ GDefS p n f1' f2' o1' o2'
replace (GCompS p s1 s2) = do
  s1' <- replace s1
  s2' <- replace s2
  return $ GCompS p s1' s2'
replace (GConvS p s) = do 
  s' <- replace s
  return $ GConvS p s'
replace (GLAdjS p s) = do 
  s' <- replace s
  return $ GLAdjS p s'
replace (GUAdjS p s) = do 
  s' <- replace s
  return $ GUAdjS p s'
replace (GLOrdS p s) = do 
  s' <- replace s
  return $ GLOrdS p s'
replace (GUOrdS p s) = do 
  s' <- replace s
  return $ GUOrdS p s'
replace s = return s

-------------------------------------------------------------------------------

