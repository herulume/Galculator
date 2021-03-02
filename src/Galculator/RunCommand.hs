
{-# LANGUAGE PatternSignatures, FlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}
 
-------------------------------------------------------------------------------
 
{- |
Module      :  Galculator.RunCommand
Description :  Evaluation of the commands.
Copyright   :  (c) Paulo Silva
License     :  LGPL
 
Maintainer  :  paufil@di.uminho.pt
Stability   :  experimental
Portability :  portable
 
-}

-------------------------------------------------------------------------------

module Galculator.RunCommand (
  runCommand,
  keepGoing
 ) where

import Control.GalcError
import qualified Control.Exception
import Control.Monad.Error
import Control.Monad.Trans
import Data.Existential
import Galculator.Evaluate
import Galculator.Proof
import Galculator.State
import Galculator.StepEval
import Language.Command.Syntax
import qualified Language.Law.Refresh as LR
import Language.Law.Syntax 
import Language.Law.SyntaxADT
import qualified Language.Law.TypeInference as LI
import qualified Language.Law.Verify as LV
import qualified Language.Module.Parser as MP
import qualified Language.Module.Refresh as MR
import Language.Module.Syntax
import Language.Module.SyntaxADT
import qualified Language.Module.TypeInference as MI
import qualified Language.Module.Verify as MV
import qualified Language.R.Refresh as RR
import Language.R.Syntax
import Language.R.SyntaxADT
import qualified Language.R.TypeInference as RI
import qualified Language.R.Verify as RV

-------------------------------------------------------------------------------

runCommand :: Command -> GalcStateT Bool
-------------------------------------------------------------------------------
runCommand Abort = 
  keepGoing . inMode [SetProofMode, ProofMode, IndirectProofMode, FinalProofMode] $
  resetProof >> galcShow "Aborted..."
-------------------------------------------------------------------------------
runCommand (Assume l) = 
  keepGoing . addAssumption =<< compileLaw l
-------------------------------------------------------------------------------
runCommand Auto =
  keepGoing . inMode [ProofMode, IndirectProofMode] $ solve
-------------------------------------------------------------------------------
runCommand (Browse mdl) = keepGoing $ do
  modules <- getLoadedModules
  when (not $ mdl `elem` modules) (throwError (ModuleRefError mdl))
  showLaws =<< getLaws mdl
  showDefinitions =<< getDefinitions mdl
  showGCs =<< getGCs mdl
-------------------------------------------------------------------------------
runCommand (Comb comb) = keepGoing . inMode [SetProofMode, ProofMode, IndirectProofMode] $
  stepEval comb
-------------------------------------------------------------------------------
runCommand (Define def) = 
  keepGoing . addDefinition =<< compileR def
-------------------------------------------------------------------------------
runCommand (Derive drv) = keepGoing . showLaw =<< evalDerivation drv
-------------------------------------------------------------------------------
runCommand Help = keepGoing . galcShow $ helpScreen
-------------------------------------------------------------------------------
runCommand Hint = keepGoing . inMode [ProofMode, IndirectProofMode] $ do
  Exists t r <- getCurExpr
  modules <- getLoadedModules 
  lws <- foldM (\res v -> (getLaws v >>= \x -> return (x++res))) [] modules
  return ()
-------------------------------------------------------------------------------
runCommand (Info _) = undefined
-------------------------------------------------------------------------------
runCommand (Load mdl) = keepGoing $ do
  modules <- getLoadedModules
  when (mdl `elem` modules) (runCommand (Unload mdl) >> return ())
  addModule mdl =<< compileModule =<< MP.parser =<< readModule mdl
  galcShow $ "Module loaded: " ++ mdl
-------------------------------------------------------------------------------
runCommand Modules = 
  keepGoing . showModules =<< getLoadedModules
-------------------------------------------------------------------------------
runCommand (Prove prf) = 
  keepGoing . inMode [GlobalMode] . setProof =<< compileLaw prf
-------------------------------------------------------------------------------
runCommand Quit = return True
-------------------------------------------------------------------------------
runCommand Reload = 
  keepGoing . inMode [GlobalMode] . 
  mapM_ (\m -> runCommand (Unload m) >> runCommand (Load m)) =<< 
  getLoadedModules
-------------------------------------------------------------------------------
runCommand Restart = 
  keepGoing . inMode [ProofMode, IndirectProofMode, FinalProofMode] $ restartProof
-------------------------------------------------------------------------------
runCommand Rules = keepGoing . galcShow $ rulesScreen
-------------------------------------------------------------------------------
runCommand Save = 
  keepGoing . inMode [FinalProofMode] $ saveProof
-------------------------------------------------------------------------------
runCommand Show = 
  keepGoing . inMode [SetProofMode, ProofMode, IndirectProofMode, FinalProofMode] $ showProof
-------------------------------------------------------------------------------
runCommand (Type expr) = keepGoing . galcShow . show =<< compileR expr
-------------------------------------------------------------------------------
runCommand (Undo mn) = 
  keepGoing . inMode [ProofMode, IndirectProofMode] . undo . maybe 1 id $ mn
-------------------------------------------------------------------------------
runCommand (Unload nm) = 
  keepGoing . inMode [GlobalMode] . removeModule $ nm
-------------------------------------------------------------------------------

solve :: GalcStateT ()
solve = galcShow "To be implemented..."

-------------------------------------------------------------------------------

showLaw :: Law -> GalcStateT ()
showLaw = galcShow . show

-------------------------------------------------------------------------------

showLaws :: [Law] -> GalcStateT ()
showLaws lst = do
  galcShow "Laws:"
  mapM_ showLaw lst

-------------------------------------------------------------------------------

showDefinitions :: [RType] -> GalcStateT ()
showDefinitions defs = do
  galcShow "Definitions:"
  mapM_ showExpr defs

-------------------------------------------------------------------------------

showExpr :: RType -> GalcStateT ()
showExpr = galcShow . show

-------------------------------------------------------------------------------

showProof :: GalcStateT ()
showProof = do
  galcShow "Current proof:"
  galcShow "---------------------------------------------------------------"
  galcShow . show =<< getExpression
  galcShow "---------------------------------------------------------------"
  galcShow . show =<< getInitialExpr
  --galcShow . show =<< getCurrentProof
  mapM_ (galcShow . sp) . reverse =<< getCurrentProof
  galcShow . show =<< getCurExpr

  where
    sp :: ProofStep -> String
    sp (RewriteStep r lrs) = show r ++ "\n\t{ " ++ concatMap slrs lrs ++ " }\n"
    sp (IndirectProof r (Left o) lps) = "indirect low:\n<" ++ concatMap sip (reverse lps) ++ ">\n"
    sp (IndirectProof r (Right o) lps) = "indirect up:\n<" ++ concatMap sip (reverse lps) ++ ">\n"
    sp QED = "Qed"

    slrs (l, _) = show l ++ "\n"
    sip (RewriteStep l lrs) = show l ++ "\n\t{ " ++ concatMap slrs lrs ++ " }\n"
    sip QED = "indirect end\n"
    sip _ = ""

-------------------------------------------------------------------------------

showModules :: [String] -> GalcStateT ()
showModules [] = galcShow "No loaded modules"
showModules mds = do
  galcShow "Loaded Modules:" 
  mapM_ galcShow mds

-------------------------------------------------------------------------------

rulesScreen :: String 
rulesScreen = 
  "--------------------------------------------------\n" ++
  "Available Rules\n" ++
  "--------------------------------------------------\n" ++
  "Shunting\t\t\tshunt\n" ++
  "Distributive (upper adjoint)\tdistr_up\n" ++
  "Distributive (lower adjoint)\tdistr_low\n" ++
  "Monotonicity (upper adjoint)\tmonot_up\n" ++
  "Monotonicity (lower adjoint)\tmonot_low\n" ++
  "Top preserving\t\t\ttop_preserving\n" ++
  "Bottom preserving\t\tbot_preserving\n" ++
  "Cancellation (upper adjoint)\tcanc_up\n" ++
  "Cancellation (lower adjoint)\tcanc_low\n" ++
  "Free theorem\t\t\tfree\n" ++
  "General law\t\t\tapply <law name>\n"

-------------------------------------------------------------------------------

showGCs :: [RType] -> GalcStateT ()
showGCs gclst = do
  galcShow "Galois connections:"
  mapM_ showExpr gclst

-------------------------------------------------------------------------------

compileR :: S -> GalcStateT RType
compileR s = do
  env <- getEnvironment
  RI.infer =<< RR.refresh =<< RV.verify env s  

-------------------------------------------------------------------------------

compileLaw :: LawS -> GalcStateT Law
compileLaw l = do
  env <- getEnvironment
  LI.infer =<< LR.refresh =<< LV.verify env l

-------------------------------------------------------------------------------

compileModule :: ModuleS -> GalcStateT Module
compileModule  = MI.infer <=< MR.refresh <=< MV.verify 

-------------------------------------------------------------------------------

readModule :: String -> GalcStateT String
readModule mdl = 
   either2error (const $ ModuleFileError mdl) <=< 
   liftIO . Control.Exception.try . readFile $ mdl ++ ".gal"
  
-------------------------------------------------------------------------------

galcShow :: String -> GalcStateT ()
galcShow = liftIO . putStrLn

-------------------------------------------------------------------------------

keepGoing :: GalcStateT () -> GalcStateT Bool
keepGoing arg = arg >> return False

-------------------------------------------------------------------------------

helpScreen :: String
helpScreen = 
  showString "--------------------------------------------------\n" .
  showString "  Commands\n" .
  showString "--------------------------------------------------\n" .
  showString "abort\t\tAborts the execution of the current proof\n" .
  showString "assume\t\tAdds an assumption to the available rules\n" .
  showString "auto\t\tTries to automatically prove the goal\n" .
  showString "browse\t\tShows the content of a given module\n" .
  showString "define\t\tAdds a new definition\n" .
  showString "derive\t\tDerives a property\n" .
  showString "help\t\tShows this help screen\n" .
  showString "hint\t\tShows a hint of the possible applicable rules\n" .
  showString "info\t\tShows information about a given identifier\n" .
  showString "load\t\tLoad a given module\n" .
  showString "modules\t\tShows the currently loaded modules\n" .
  showString "prove\t\tIntroduces a goal to prove\n" .
  showString "quit\t\tExists Galculator\n" .
  showString "reload\t\tReloads all modules\n" .
  showString "restart\t\tRestarts the current proof\n" .
  showString "rules\t\tLists the available rules\n" .
  showString "save\t\tSaves the current proof\n" .
  showString "show\t\tShows the current proof\n" .
  showString "type\t\tShows the type of a given expression\n" . 
  showString "undo\t\tUndos a proof step\n" .
  showString "unload\t\tUnloads a given module" $ ""

-------------------------------------------------------------------------------

