
{-# LANGUAGE MultiParamTypeClasses, PatternSignatures, FlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}

-------------------------------------------------------------------------------

{- |
Module      :  Galculator.State
Description :  Global state of Galculator.
Copyright   :  (c) Paulo Silva
License     :  LGPL

Maintainer  :  paufil@di.uminho.pt
Stability   :  experimental
Portability :  portable

This module needs some improvements.

-}

-------------------------------------------------------------------------------

module Galculator.State (
  GalcState(..),
  GalcStateT,
  GalcSt,
  emptyState,
 
  OpMode(..),
  getOpMode,
  inMode,

  updateProof,
  enterProofMode,
  enterFinalProofMode,
  enterIndirectProofMode,

  setProof,
  resetProof,

  getEnvironment,
  addAssumption,
  getLoadedModules,
  getLaws,
  getLaw,
  getDefinitions,
  addDefinition,
  addModule,

  getCurExpr,
  saveProof,
  restartProof,

  undo,
  removeModule,
  getGC,
  getGCs,
  getAssumptions,

  getCurrentProof,
  getExpression,

  getInitialExpr
 ) where

import Control.GalcError
import Control.Monad.Error
import Control.Monad.Fresh
import Control.Monad.State
import Control.Monad.Trans
import Data.Env
import Data.Existential
import Data.Maybe
import Data.IORef
import Galculator.Proof
import Language.Law.Syntax
import Language.Module.Syntax
import Language.R.Syntax


-------------------------------------------------------------------------------

type GalcStateT t = ErrorT GalcError (FreshT [String] GalcState) t

-------------------------------------------------------------------------------

data GalcState a = GalcState {
  unGalcState :: IORef GalcSt -> IO a }

-------------------------------------------------------------------------------

data GalcSt = GalcSt {
  opMode           :: OpMode,

  modules          :: Env Module,
  rtAssumpt        :: Env Law,
  rtDefinitions    :: Env RType,

  proof            :: Maybe ProofSt
 }

-------------------------------------------------------------------------------

data OpMode = 
    GlobalMode 
  | SetProofMode 
  | ProofMode 
  | IndirectProofMode 
  | FinalProofMode
  deriving Eq

-------------------------------------------------------------------------------

getOpMode :: GalcStateT OpMode
getOpMode = ggets opMode

-------------------------------------------------------------------------------

enterGlobalMode :: GalcStateT ()
enterGlobalMode = updateGalcState $ \st -> st {opMode = GlobalMode}

-------------------------------------------------------------------------------

enterSetProofMode :: GalcStateT ()
enterSetProofMode = updateGalcState $ \st -> st {opMode = SetProofMode}

-------------------------------------------------------------------------------

enterProofMode :: GalcStateT ()
enterProofMode = updateGalcState $ \st -> st {opMode = ProofMode}

-------------------------------------------------------------------------------

enterIndirectProofMode :: GalcStateT ()
enterIndirectProofMode = 
  updateGalcState $ \st -> st {opMode = IndirectProofMode}

-------------------------------------------------------------------------------

enterFinalProofMode :: GalcStateT ()
enterFinalProofMode = updateGalcState $ \st -> st {opMode = FinalProofMode}

-------------------------------------------------------------------------------
inMode :: [OpMode] -> GalcStateT () -> GalcStateT ()
inMode lmod a = do
  cmod <- getOpMode
  if cmod `elem` lmod then a else throwError ModeError

-------------------------------------------------------------------------------

instance Monad GalcState where
  (GalcState m) >>= k = GalcState $ \r -> m r >>= \a -> unGalcState (k a) r
  return a = GalcState $ \_ -> return a

-------------------------------------------------------------------------------

instance MonadIO GalcState where
  liftIO m = GalcState $ \_ -> m >>= return -- !??

-------------------------------------------------------------------------------

instance MonadState GalcSt GalcState where
  get   = GalcState $ \r -> readIORef r
  put s = GalcState $ \r -> writeIORef r s

-------------------------------------------------------------------------------

glift :: GalcState a -> GalcStateT a
glift = lift . lift 

-------------------------------------------------------------------------------
gget :: GalcStateT GalcSt
gget = glift get
-------------------------------------------------------------------------------
gput :: GalcSt -> GalcStateT ()
gput = glift . put
-------------------------------------------------------------------------------
ggets :: (GalcSt -> a) -> GalcStateT a
ggets = glift . gets
-------------------------------------------------------------------------------

emptyState :: GalcSt
emptyState = 
  GalcSt {
    opMode           = GlobalMode,
    modules          = emptyEnv,
    rtAssumpt        = emptyEnv,
    rtDefinitions    = emptyEnv,

    proof            = Nothing
  }

-------------------------------------------------------------------------------

updateGalcState :: (GalcSt -> GalcSt) ->  GalcStateT ()
updateGalcState f = glift . GalcState $ \r -> modifyIORef r f

-------------------------------------------------------------------------------
-- | Updates the proof information.
updateProof :: (ProofSt -> GalcStateT ProofSt) -> GalcStateT ()
updateProof f = do
  st   <- gget
  prf  <- maybe2error NoProofError . proof $ st
  prf' <- f prf
  gput $ st {proof = Just prf'}

-------------------------------------------------------------------------------
-- | Sets the law to prove and enters the set proof mode.
setProof :: Law -> GalcStateT ()
setProof e = (updateGalcState $
  \st -> st { proof = Just $ ProofSt (getName e) e (Left (getLeft e)) [] }) 
  >> enterSetProofMode

-------------------------------------------------------------------------------
-- | Removes all the proof information and entes the global mode.
resetProof :: GalcStateT ()
resetProof = 
  (updateGalcState $ \st -> st { proof = Nothing }) >> enterGlobalMode

-------------------------------------------------------------------------------
-- | Returns the environment where expressions can be evaluated.
--   The environment is composed of definitions, galois connections and 
--   run-definitions.
getEnvironment :: GalcStateT (Env RType)
getEnvironment = do
  mds <- ggets modules
  let defs = map definitions . values $ mds
      gcl =  map gcs . values $ mds 
  rtd <- ggets rtDefinitions
  return $ foldr joinEnv rtd (defs ++ gcl)

-------------------------------------------------------------------------------
-- | Adds an assumption (law) into the run-time assumption environment.
addAssumption :: Law -> GalcStateT ()
addAssumption l = do
  let nm = getName l
  st <- gget
  lookupGuard ExistingRefError nm (rtAssumpt st)
  gput $ st {rtAssumpt = addEnv nm l (rtAssumpt st)}
  
-------------------------------------------------------------------------------
-- | Returns the name of the loaded modules.
getLoadedModules :: GalcStateT [String]
getLoadedModules = return . indexes =<< ggets modules

-------------------------------------------------------------------------------
-- | Returns the list of laws of a given module.
getLaws :: String -> GalcStateT [Law]
getLaws mdl = 
  return . values . laws =<< lookupE ModuleRefError mdl =<< ggets modules

-------------------------------------------------------------------------------
-- | Given a name, returns the law definitions.
getLaw :: String -> GalcStateT Law
getLaw nm = 
  maybe2error (ReferenceError nm) . msum . map (lookupEnv nm . laws) . values 
  =<< ggets modules

-------------------------------------------------------------------------------
-- | Returns the list of definitions of a given module.
getDefinitions :: String -> GalcStateT [RType]
getDefinitions mdl = 
  return . values . definitions =<< lookupE ModuleRefError mdl =<< ggets modules

-------------------------------------------------------------------------------
-- | Adds a definitions into the run-time definition environment.
addDefinition :: RType -> GalcStateT ()
addDefinition def = do
  let nm = defName def
  st <- gget
  lookupGuard ExistingRefError nm (rtDefinitions st)
  gput $ st { rtDefinitions = addEnv nm def (rtDefinitions st) }
  
-------------------------------------------------------------------------------
-- Repeated:
-- Language.Module.TypeInference
defName :: RType -> String
defName (Exists _ (DEF n _)) = n
defName _ = ""

-------------------------------------------------------------------------------
-- | Returns the current expression in the proof.
getCurExpr :: GalcStateT RType
getCurExpr = 
  return . eitherId . curExpr =<< maybe2error NoProofError =<< ggets proof

eitherId :: Either a a -> a
eitherId = either id id

-------------------------------------------------------------------------------
-- | Adds a new module into the module environement
addModule :: String -> Module -> GalcStateT ()
addModule nm m = do
  st <- gget
  lookupGuard ExistingRefError nm (modules st)
  gput $ st { modules = addEnv nm m (modules st) }

-------------------------------------------------------------------------------

saveProof :: GalcStateT ()
saveProof = (updateGalcState $
  \st -> st ) >> enterGlobalMode

-------------------------------------------------------------------------------

undo :: Int -> GalcStateT ()
undo n = updateProof $ return

-------------------------------------------------------------------------------
-- | Restarts a proof, assuming a left expression.
restartProof :: GalcStateT ()
restartProof = (updateProof $ \prf ->
    return prf {curExpr = Left . getLeft . expression $ prf, curProof = []}) >> 
  enterSetProofMode

-------------------------------------------------------------------------------
-- | Removes a module from the module environment.
removeModule :: String -> GalcStateT ()
removeModule nm = do
  st <- gget
  lookupE ModuleRefError nm (modules st)
  gput $ st { modules = delete nm (modules st) }
 
-------------------------------------------------------------------------------

-- TODO: MonadOr?
getGC :: MonadPlus m => String -> GalcStateT (m RType)
getGC nm = return . msum . map (lookupEnv nm . gcs) . values =<< ggets modules

getGCs :: String -> GalcStateT [RType]
getGCs nm = return . msum . map (lookupEnv nm . gcs) . values =<< ggets modules

getAssumptions :: GalcStateT [Law]
getAssumptions = return . values =<< ggets rtAssumpt

getAssumption :: String -> GalcStateT Law
getAssumption nm = lookupE ReferenceError nm =<< ggets rtAssumpt

---  ======================
getExpression :: GalcStateT Law
getExpression = return . expression =<< maybe2error NoProofError =<< ggets proof

getInitialExpr :: GalcStateT RType
getInitialExpr = 
  return . getLeft . expression =<< maybe2error NoProofError =<< ggets proof

getCurrentProof :: GalcStateT Proof
getCurrentProof = 
  return . curProof =<< maybe2error NoProofError =<< ggets proof
-------------------------------------------------------------------------------

lookupGuard :: MonadError GalcError m 
        => (String -> GalcError) -> String -> Env a -> m ()
lookupGuard err nm = 
  maybe (return ()) (const $ throwError (err nm)) . lookupEnv nm

-------------------------------------------------------------------------------

lookupE :: MonadError GalcError m 
        => (String -> GalcError) -> String -> Env a -> m a
lookupE err nm = maybe2error (err nm) . lookupEnv nm 

-------------------------------------------------------------------------------


