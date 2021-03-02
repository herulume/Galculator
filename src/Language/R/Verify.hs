
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}

-------------------------------------------------------------------------------

{- |
Module      :  Language.R.Verify
Description :  Validation of the uniqueness of definitions and the existence 
               of references.
Copyright   :  (c) Paulo Silva
License     :  LGPL

Maintainer  :  paufil@di.uminho.pt
Stability   :  experimental
Portability :  portable

-}

-------------------------------------------------------------------------------

module Language.R.Verify (
  verify,
  getDefs,
  replaceDefs,
  Env'(..),
  ExtEnv,
  reps
 ) where

-------------------------------------------------------------------------------

import Control.GalcError
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Data.Env
import Data.List
import Language.R.Syntax
import Language.R.SyntaxADT

-------------------------------------------------------------------------------

-- ========
type ExtEnv = Env RType
type IntEnv = Env S
data Env' = Env' {internal :: IntEnv, external :: ExtEnv }


reps :: Ord a => [a] -> [a]
reps = map head . filter ((>1) . length ) . group . sort
-- ========
-------------------------------------------------------------------------------

verify :: MonadError GalcError m => ExtEnv -> S -> m S
verify extEnv s = let 
    env = getDefs s
    ids = reps $ indexes extEnv ++ map fst env
  in if null ids 
     then runReaderT (replaceDefs s) $ Env' {internal = fromListEnv env, 
                                             external = extEnv }
     else throwError $ MultiDefError . concatMap ((++"\n") . show) $ ids

-------------------------------------------------------------------------------

getDefs :: S -> [(String, S)]
getDefs d@(DefS _ n _) = [(n,d)]
getDefs g@(GDefS _ n f1 f2 o1 o2) = 
  (n,g) : getDefs f1 ++ getDefs f2 ++ getDefs o1 ++ getDefs o2
getDefs (MeetS _ s1 s2) = getDefs s1 ++ getDefs s2
getDefs (JoinS _ s1 s2) =  getDefs s1 ++ getDefs s2
getDefs (ConvS _ s) = getDefs s 
getDefs (CompS _ s1 s2) = getDefs s1 ++ getDefs s2
getDefs (SplitS _ s1 s2) = getDefs s1 ++ getDefs s2
getDefs (OrdS _ s) = getDefs s 
getDefs (FunS _ s) = getDefs s  
getDefs (LeftsecS _ s1 s2) = getDefs s1 ++ getDefs s2
getDefs (RightsecS _ s1 s2) = getDefs s1 ++ getDefs s2
getDefs (ApplyS _ s1 s2) = getDefs s1 ++ getDefs s2 
getDefs (ProdS _ s1 s2) = getDefs s1 ++ getDefs s2
getDefs (EitherS _ s1 s2) = getDefs s1 ++ getDefs s2
getDefs (MaybeS _ s) = getDefs s  
getDefs (ListS _ s) = getDefs s  
getDefs (SetS _ s) = getDefs s  
getDefs (MapS _ s) = getDefs s  
getDefs (ReynoldsS _ s1 s2) = getDefs s1 ++ getDefs s2
getDefs (FCompS _ s1 s2) = getDefs s1 ++ getDefs s2
getDefs (OCompS _ s1 s2) = getDefs s1 ++ getDefs s2
getDefs (OConvS _ s) = getDefs s 
getDefs (OProdS _ s) = getDefs s 
getDefs (OJoinS _ s) = getDefs s 
getDefs (OMeetS _ s) = getDefs s 
getDefs (OMaxS _ s) = getDefs s 
getDefs (OMinS _ s) = getDefs s 
getDefs (GCompS _ s1 s2) = getDefs s1 ++ getDefs s2 
getDefs (GConvS _ s) = getDefs s 
getDefs (GLAdjS _ s) = getDefs s 
getDefs (GUAdjS _ s) = getDefs s 
getDefs (GLOrdS _ s) = getDefs s 
getDefs (GUOrdS _ s) = getDefs s 
getDefs _ = []

-------------------------------------------------------------------------------

replaceDefs :: (MonadError GalcError m, MonadReader Env' m) 
            => S -> m S
replaceDefs (RefS p n) = do -- TODO position in the substitution
  extEnv <- asks external
  maybe (maybe (throwError $ ReferenceError n) (return . RefExtS p) . 
         lookupEnv n $ extEnv) 
         return . lookupEnv n =<< asks internal
replaceDefs (NegS p s) = do 
 s' <- replaceDefs s
 return $ NegS p s'
replaceDefs (MeetS p s1 s2) = do 
 s1' <- replaceDefs s1
 s2' <- replaceDefs s2
 return $ MeetS p s1' s2' 
replaceDefs (JoinS p s1 s2) = do 
 s1' <- replaceDefs s1
 s2' <- replaceDefs s2
 return $ JoinS p s1' s2'
replaceDefs (ConvS p s) = do 
 s' <- replaceDefs s
 return $ ConvS p s'
replaceDefs (CompS p s1 s2) = do 
 s1' <- replaceDefs s1
 s2' <- replaceDefs s2
 return $ CompS p s1' s2'
replaceDefs (SplitS p s1 s2) = do 
 s1' <- replaceDefs s1
 s2' <- replaceDefs s2
 return $ SplitS p s1' s2'
replaceDefs (OrdS p s) = do 
 s' <- replaceDefs s
 return $ OrdS p s'
replaceDefs (FunS p s) = do 
 s' <- replaceDefs s
 return $ FunS p s'
replaceDefs (LeftsecS p s1 s2) = do 
 s1' <- replaceDefs s1
 s2' <- replaceDefs s2
 return $ LeftsecS p s1' s2'
replaceDefs (RightsecS p s1 s2) = do 
 s1' <- replaceDefs s1
 s2' <- replaceDefs s2
 return $ RightsecS p s1' s2'
replaceDefs (ApplyS p s1 s2) = do 
 s1' <- replaceDefs s1
 s2' <- replaceDefs s2
 return $ ApplyS p s1' s2'
replaceDefs (ProdS p s1 s2) = do 
 s1' <- replaceDefs s1
 s2' <- replaceDefs s2
 return $ ProdS p s1' s2'
replaceDefs (EitherS p s1 s2) = do 
 s1' <- replaceDefs s1
 s2' <- replaceDefs s2
 return $ EitherS p s1' s2'
replaceDefs (MaybeS p s) = do 
 s' <- replaceDefs s
 return $ MaybeS p s'
replaceDefs (ListS p s) = do 
 s' <- replaceDefs s
 return $ ListS p s'
replaceDefs (SetS p s) = do 
 s' <- replaceDefs s
 return $ SetS p s'
replaceDefs (MapS p s) = do 
 s' <- replaceDefs s
 return $ MapS p s'
replaceDefs (ReynoldsS p s1 s2) = do
 s1' <- replaceDefs s1
 s2' <- replaceDefs s2
 return $ ReynoldsS p s1' s2'
replaceDefs (FCompS p s1 s2) = do 
 s1' <- replaceDefs s1
 s2' <- replaceDefs s2
 return $ FCompS p s1' s2'
replaceDefs (OCompS p s1 s2) = do 
 s1' <- replaceDefs s1
 s2' <- replaceDefs s2
 return $ OCompS p s1' s2'
replaceDefs (OConvS p s) = do 
 s' <- replaceDefs s
 return $ OConvS p s'
replaceDefs (OProdS p s) = do 
 s' <- replaceDefs s
 return $ OProdS p s'
replaceDefs (OJoinS p s) =  do 
 s' <- replaceDefs s
 return $ OJoinS p s'
replaceDefs (OMeetS p s) = do 
 s' <- replaceDefs s
 return $ (OMeetS p s')
replaceDefs (OMaxS p s) = do 
 s' <- replaceDefs s
 return $ (OMaxS p s')
replaceDefs (OMinS p s) = do 
 s' <- replaceDefs s
 return $ (OMinS p s')
replaceDefs (GDefS p n f1 f2 o1 o2) = do
  f1' <- replaceDefs f1
  f2' <- replaceDefs f2
  o1' <- replaceDefs o1
  o2' <- replaceDefs o2
  return $ GDefS p n f1' f2' o1' o2'
replaceDefs (GCompS p s1 s2) = do 
 s1' <- replaceDefs s1
 s2' <- replaceDefs s2
 return $ GCompS p s1' s2'
replaceDefs (GConvS p s) = do 
 s' <- replaceDefs s
 return $ GConvS p s'
replaceDefs (GLAdjS p s) = do 
 s' <- replaceDefs s
 return $ GLAdjS p s'
replaceDefs (GUAdjS p s) = do 
 s' <- replaceDefs s
 return $ GUAdjS p s'
replaceDefs (GLOrdS p s) = do 
 s' <- replaceDefs s
 return $ GLOrdS p s'
replaceDefs (GUOrdS p s) = do 
 s' <- replaceDefs s
 return $ GUOrdS p s'
replaceDefs s = return s

-------------------------------------------------------------------------------