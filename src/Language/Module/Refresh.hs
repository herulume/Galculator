
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}

-------------------------------------------------------------------------------

{- |
Module      :  Language.Module.Refresh
Description :  Operations for refreshing the variable names.
Copyright   :  (c) Paulo Silva
License     :  LGPL

Maintainer  :  paufil@di.uminho.pt
Stability   :  experimental
Portability :  portable

-}

-------------------------------------------------------------------------------

module Language.Module.Refresh (
  refresh
 ) where

import Control.Monad.Fresh
import qualified Language.Law.Refresh as L
import Language.Module.SyntaxADT
import qualified Language.R.Refresh as R

-------------------------------------------------------------------------------

refresh :: MonadFresh [String] String m => ModuleS -> m ModuleS
refresh (ModuleS nm laws gcs defs) = do
   laws' <- mapM L.refresh laws
   gcs'  <- mapM R.refresh gcs
   defs' <- mapM R.refresh defs
   return $ ModuleS nm laws' gcs' defs'

-------------------------------------------------------------------------------

