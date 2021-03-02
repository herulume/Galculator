
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall #-}

-------------------------------------------------------------------------------

{- |
Module      :  Language.Command.Syntax
Description :  Abstract representation of commands.
Copyright   :  (c) Paulo Silva
License     :  LGPL
  
Maintainer  :  paufil@di.uminho.pt
Stability   :  experimental
Portability :  portable
  
-}
  
-------------------------------------------------------------------------------

module Language.Command.Syntax (
  Command(..),
  commands
 ) where

import Language.Derivation.Syntax
import Language.Law.SyntaxADT
import Language.R.SyntaxADT
import Language.Step.Syntax
import Language.Type.Syntax()

-------------------------------------------------------------------------------

data Command where
  Abort       :: Command
  Assume      :: LawS -> Command
  Auto        :: Command
  Browse      :: String -> Command
  Comb        :: Step -> Command
  Define      :: S -> Command
  Derive      :: Derivation -> Command
  Help        :: Command
  Hint        :: Command
  Info        :: String -> Command
  Load        :: String -> Command
  Modules     :: Command
  Prove       :: LawS -> Command
  Quit        :: Command
  Reload      :: Command
  Restart     :: Command
  Rules       :: Command
  Save        :: Command
  Show        :: Command
  Type        :: S -> Command
  Undo        :: Maybe Int -> Command
  Unload      :: String -> Command
  deriving Show

-------------------------------------------------------------------------------

commands :: [String]
commands = [
  "abort",
  "assume",
  "auto",
  "browse",
  "define",
  "derive",
  "help",
  "hint",
  "info",
  "load",
  "modules",
  "prove",
  "quit",
  "reload",
  "restart",
  "rules",
  "save",
  "show",
  "type",
  "undo",
  "unload"
 ]

-------------------------------------------------------------------------------

