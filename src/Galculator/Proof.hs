
{-# LANGUAGE TypeSynonymInstances, FlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}

-------------------------------------------------------------------------------
 
{- |
Module      :  Galculator.Proof
Description :  
Copyright   :  (c) Paulo Silva
License     :  LGPL
 
Maintainer  :  paufil@di.uminho.pt
Stability   :  experimental
Portability :  portable
 
-}

-------------------------------------------------------------------------------

module Galculator.Proof (
  Proof,
  ProofSt(..),
  ProofStep(..),
  addStep,
  getOrd
 ) where

import Control.GalcError
import Control.Monad.Error
import Galculator.Rule
import Language.Law.Syntax
import Language.R.Syntax

-------------------------------------------------------------------------------

data ProofSt = ProofSt {
  name :: String,
  expression :: Law,
  curExpr :: Either RType RType,
  curProof :: Proof
 }

type Proof = [ProofStep]

instance Show Proof where
  show = pretty

-------------------------------------------------------------------------------

data ProofStep = 
    RewriteStep RType [RewriteStep]
  | IndirectProof RType (Either RType RType) [ProofStep]
  | QED

-------------------------------------------------------------------------------

getOrd :: MonadError GalcError m => Proof -> m (Either RType RType)
getOrd [] = throwError NoOrderSetError
getOrd (x:_) = case x of
  IndirectProof _ o _ -> return o
  _ -> throwError NoOrderSetError


addStep :: ProofStep -> Proof -> Proof
addStep stp [] = [stp]
addStep stp (x:xs) = 
  case x of
    IndirectProof e o lst -> 
      if null lst 
      then IndirectProof e o [stp] : xs
      else case head lst of
             QED -> stp : x : xs
             _ -> IndirectProof e o (stp:lst) : xs
    _ -> stp : x : xs

-------------------------------------------------------------------------------

pretty :: Proof -> String
pretty = 
  concatMap sp . reverse
  where
    sp :: ProofStep -> String
    sp (RewriteStep r lrs) = show r ++ "\n\t{ " ++ concatMap slrs lrs ++ " }\n"
    sp (IndirectProof r (Left o) lps) = "indirect low:\n<" ++ concatMap sip (reverse lps) ++ ">\n"
    sp (IndirectProof r (Right o) lps) = "indirect up:\n<" ++ concatMap sip (reverse lps) ++ ">\n"
    sp QED = "Qed"

    slrs (l, _) = "\t  " ++ show l ++ "\n"
    sip (RewriteStep l lrs) = show l ++ "\n\t{ " ++ concatMap slrs lrs ++ " }\n"
    sip QED = "indirect end\n"
    sip _ = ""

-------------------------------------------------------------------------------
