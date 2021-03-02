
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}
 
-------------------------------------------------------------------------------

{- |
Module      :  Language.Module.Verify
Description :  Validation of the uniqueness of definitions and the existence 
               of references. 
Copyright   :  (c) Paulo Silva
License     :  LGPL
  
Maintainer  :  paufil@di.uminho.pt
Stability   :  experimental
Portability :  portable
 
-}

-------------------------------------------------------------------------------

module Language.Module.Verify (
  verify
 ) where

import Control.GalcError
import Control.Monad.Error
import Control.Monad.Reader
import Data.Env
import Language.Law.SyntaxADT
import qualified Language.Law.Verify as L
import Language.Module.SyntaxADT
import qualified Language.R.Verify as R

-------------------------------------------------------------------------------

verify :: MonadError GalcError m => ModuleS -> m ModuleS
verify (ModuleS nm laws gcs defs) = let 
   defs'  = concatMap R.getDefs defs
   defg   = concatMap R.getDefs gcs
   defl   = concatMap L.getDefs laws
   intEnv = defs' ++ defg ++ defl
   lname = R.reps . map getName $ laws
   ids = R.reps . map fst $ intEnv
  in if null ids && null lname
     then runReaderT (do 
         lawsR <- mapM L.replaceDefs laws
         gcsR <-  mapM R.replaceDefs gcs
         defsR <- mapM R.replaceDefs defs 
         return $ ModuleS nm lawsR gcsR defsR) $ 
            R.Env' {R.internal = fromListEnv intEnv, R.external = emptyEnv}
     else if null lname then throwError $ aux ids
                        else throwError $ aux lname
  where
    aux = MultiDefError . concatMap ((++"\n") . show)

-------------------------------------------------------------------------------
