
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}

-------------------------------------------------------------------------------

{- |
Module      :  Language.Law.Refresh
Description :  Operations for refreshing the variable names.
Copyright   :  (c) Paulo Silva
License     :  LGPL

Maintainer  :  paufil@di.uminho.pt
Stability   :  experimental
Portability :  portable

-}

-------------------------------------------------------------------------------

module Language.Law.Refresh (
  refresh
 ) where

import Control.Monad.Fresh
import Control.Monad.Reader
import Data.List
import Language.Law.SyntaxADT
import qualified Language.R.Refresh as R
import Language.R.SyntaxADT

-------------------------------------------------------------------------------

refresh :: MonadFresh [String] String m => LawS -> m LawS
refresh (EquivS p n s1 s2) = aux (EquivS p n) s1 s2
refresh (ImplS p n s1 s2)  = aux (ImplS p n) s1 s2

-------------------------------------------------------------------------------

aux :: MonadFresh [String] String m => (S -> S -> LawS) -> S -> S -> m LawS
aux constr s1 s2 = let
    (rvar1,tvar1) = R.collect s1
    (rvar2,tvar2) = R.collect s2
    rvar = nub $ rvar1 ++ rvar2
    tvar = nub $ tvar1 ++ tvar2
  in do
    newVars  <- R.refreshVar rvar 
    newTypes <- R.refreshType tvar 
    let s1' = runReader (R.replace s1) $ R.St {R.rvars = newVars, 
                                               R.tvars = newTypes}
        s2' = runReader (R.replace s2) $ R.St {R.rvars = newVars, 
                                               R.tvars = newTypes}
    return $ constr s1' s2' 

-------------------------------------------------------------------------------

