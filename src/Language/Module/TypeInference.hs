
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}
 
-------------------------------------------------------------------------------

{- |
Module      :  Language.Module.TypeInference
Description :  Type inference for the module representation.
Copyright   :  (c) Paulo Silva
License     :  LGPL
  
Maintainer  :  paufil@di.uminho.pt
Stability   :  experimental
Portability :  portable
 
-}

-------------------------------------------------------------------------------

module Language.Module.TypeInference (
  infer 
 ) where

import Control.GalcError
import Control.MonadOr
import Control.Monad.Error
import Control.Monad.Fresh
import Data.Existential
import qualified Data.Map as Map
import qualified Language.Law.Syntax as LS
import qualified Language.Law.TypeInference as L
import Language.Module.Syntax
import Language.Module.SyntaxADT
import qualified Language.R.TypeInference as R
import Language.R.Syntax

-------------------------------------------------------------------------------

infer :: (MonadError GalcError m, MonadFresh [String] String m, MonadOr m) 
      => ModuleS -> m Module
infer (ModuleS nm laws' gcs' defs') = do
  laws'' <- mapM L.infer laws'
  gcs''  <- mapM R.infer gcs'
  defs'' <- mapM R.infer defs'
  return $ Module {
    name = nm, 
    laws = Map.fromList . map (\x -> (LS.getName x, x)) $ laws'', 
    gcs = Map.fromList . map (\x -> (gcName x, x)) $ gcs'', 
    definitions = Map.fromList . map (\x -> (defName x, x)) $ defs'' }

-------------------------------------------------------------------------------

gcName :: RType -> String
gcName (Exists _ (GDef n _ _ _ _)) = n
gcName _ = ""

-------------------------------------------------------------------------------

defName :: RType -> String
defName (Exists _ (DEF n _)) = n
defName _ = ""

-------------------------------------------------------------------------------
