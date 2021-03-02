
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}

-------------------------------------------------------------------------------

{- |
Module      :  Language.Law.Verify
Description :  Validation of the uniqueness of definitions and the existence 
               of references.
Copyright   :  (c) Paulo Silva
License     :  LGPL

Maintainer  :  paufil@di.uminho.pt
Stability   :  experimental
Portability :  portable

-}

-------------------------------------------------------------------------------

module Language.Law.Verify (
  verify,
  getDefs,
  replaceDefs
 ) where

import Control.GalcError
import Control.Monad.Error
import Control.Monad.Reader
import Data.Env
import Language.Law.SyntaxADT
import Language.R.SyntaxADT
import qualified Language.R.Verify as R

-------------------------------------------------------------------------------

verify :: MonadError GalcError m => R.ExtEnv -> LawS -> m LawS
verify extEnv l = let 
    intEnv = getDefs l
    ids = R.reps $ indexes extEnv ++ map fst intEnv
    env = R.Env' {R.internal = fromListEnv intEnv, R.external = extEnv}
  in if null ids
     then runReaderT (replaceDefs l) env
     else throwError $ MultiDefError . concatMap ((++"\n") . show) $ ids
  
-------------------------------------------------------------------------------

getDefs :: LawS -> [(String, S)]
getDefs (EquivS _ _ s1 s2) = R.getDefs s1 ++ R.getDefs s2
getDefs (ImplS  _ _ s1 s2) = R.getDefs s1 ++ R.getDefs s2

-------------------------------------------------------------------------------

replaceDefs :: (MonadError GalcError m, MonadReader R.Env' m) 
            => LawS -> m LawS
replaceDefs (EquivS p n s1 s2) = do
  s1' <- R.replaceDefs s1
  s2' <- R.replaceDefs s2
  return $ EquivS p n s1' s2'
replaceDefs (ImplS p n s1 s2) = do
  s1' <- R.replaceDefs s1
  s2' <- R.replaceDefs s2
  return $ ImplS p n s1' s2'

-------------------------------------------------------------------------------

