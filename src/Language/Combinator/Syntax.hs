
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall #-}

-------------------------------------------------------------------------------

{- |
Module      :  Language.Combinator.Syntax
Description :  Abstract representation of strategic rewriting combinators.
Copyright   :  (c) Paulo Silva
License     :  LGPL
  
Maintainer  :  paufil@di.uminho.pt
Stability   :  experimental
Portability :  portable
  
-}
  
-------------------------------------------------------------------------------

module Language.Combinator.Syntax (
  Combinator(..),
  combinators
 ) where

import Language.Derivation.Syntax

-------------------------------------------------------------------------------

data Combinator where
  Nop         :: Combinator
  Fail        :: Combinator
  Seq         :: Combinator -> Combinator -> Combinator
  Choice      :: Combinator -> Combinator -> Combinator
  LChoice     :: Combinator -> Combinator -> Combinator
  Many        :: Combinator -> Combinator
  Many1       :: Combinator -> Combinator
  Try         :: Combinator -> Combinator
  Once        :: Combinator -> Combinator
  Everywhere  :: Combinator -> Combinator
  Everywhere' :: Combinator -> Combinator
  Innermost   :: Combinator -> Combinator
  All         :: Combinator -> Combinator
  One         :: Combinator -> Combinator
  Rule        :: Derivation -> Combinator
  deriving Show
  
-------------------------------------------------------------------------------

combinators :: [String]
combinators = [
  "nop", "fail", "seq", "choice", "lchoice", "many", "many1", "try", "once",
  "everywhere", "everywhere'", "innermost", "all", "one" 
 ]

-------------------------------------------------------------------------------
