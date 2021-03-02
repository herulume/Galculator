
{-# OPTIONS_GHC -Wall #-}

-------------------------------------------------------------------------------

{- |
Module      :  Language.R.Pretty
Description :  Pretty-printer of the expression representatation.
Copyright   :  (c) Paulo Silva
License     :  LGPL
 
Maintainer  :  paufil@di.uminho.pt
Stability   :  experimental
Portability :  portable

-}

-------------------------------------------------------------------------------

module Language.R.Pretty (
  showR
 ) where

import {-# SOURCE #-} Language.R.Syntax
import Language.Type.Pretty

-------------------------------------------------------------------------------

showR :: R r -> ShowS
showR = pretty
{-
showR BOT = showString "BOT"
showR TOP = showString "TOP"
showR (NEG r) = 
  parens $ 
    showString "NEG " .
    showR r
showR (MEET r s) = 
  parens $
    showString "MEET " .
    showR r .
    space . 
    showR s
showR (JOIN r s) = 
  parens $ 
    showString "JOIN " .
    showR r .
    space .
    showR s
showR ID = showString "ID"
showR (CONV r) =
  parens $
    showString "CONV " .
    showR r
showR (COMP _ r s) =
  parens $
    showString "COMP " .
    showR r .
    space .
    showR s
showR (SPLIT r s) =
  parens $
    showString "SPLIT " .
    showR r .
    space .
    showR s
showR (ORD o) =
  parens $ showString "ORD " . showR o
showR (FUN f) =
  parens $ showString "FUN " . showR f
showR (LEFTSEC _ f s) =
  parens $
    showString "LEFTSEC " .
    showR f .
    space .
    showR s
showR (RIGHTSEC _ f s) =
   parens $
     showString "RIGHTSEC " .
     showR f .
     space .
     showR s
showR (APPLY _ r v) = 
  parens $
    showString "APPLY " .
    showR r .
    space .
    showR  v 
showR (DEF nm t) = 
  parens $
    showString "DEF " .
    showString nm .
    space .
    showType t
showR (Var nm) = parens $ showString "Var " . showString nm
showR (PROD r s) = 
  parens $
    showString "PROD " .
    showR r .
    space .
    showR s
showR (EITHER r s) = 
  parens $
    showString "EITHER " .
    showR r .
    space .
    showR s
showR (MAYBE r) =
  parens $ 
    showString "MAYBE " .
    showR r
showR (LIST r) =
  parens $ 
    showString "LIST " .
    showR r
showR (SET r) =
  parens $ 
    showString "SET " .
    showR r
showR (MAP r) =
  parens $ 
    showString "MAP " .
    showR r
showR (REYNOLDS r s) = 
  parens $
    showString "REYNOLDS " .
    showR r .
    space .
    showR s
showR FId = showString "FId"
showR (FComp _ f g) = 
  parens $
    showString "FComp " .
    showR f .
    space .
    showR g
showR OId = showString "OId"
showR (OComp o1 o2) = 
  parens $
    showString "OComp " .
    showR o1 .
    space .
    showR o2
showR (OConv o) = 
  parens $
    showString "OConv " .
    showR o
showR (OProd o) =
  parens $
    showString "OProd " .
    showR o
showR (OJoin o) = 
  parens $
    showString "OJoin " .
    showR o
showR (OMeet o) = 
  parens $
    showString "OMeet " .
    showR o
showR (OMax o) = 
  parens $
    showString "OMax " .
    showR o
showR (OMin o) = 
  parens $
    showString "OMin " .
    showR o
showR (GDef nm f g fo go) = 
  parens $
    showString "GDef " .
    showString nm . space .
    showR f . space . showR g . space .
    showR fo . space . showR go
showR GId = showString "GId"
showR (GComp _ g1 g2) = 
  parens $ 
    showString "GComp " .
    showR g1 .
    space .
    showR g2
showR (GConv g) = 
  parens $
    showString "GConv " .
    showR g
showR (GLAdj g) = 
  parens $
    showString "GLAdj " .
    showR g
showR (GUAdj g) = 
  parens $
    showString "GUAdj " .
    showR g
showR (GLOrd _ g) =
  parens $
    showString "GLOrd " .
    showR g
showR (GUOrd _ g) = 
  parens $
    showString "GUOrd " .
    showR g
-}
-------------------------------------------------------------------------------

parens :: ShowS -> String -> String
parens = showParen True 

-------------------------------------------------------------------------------

space :: ShowS
space = showChar ' '

-------------------------------------------------------------------------------

pretty :: R r -> ShowS
pretty BOT = showString "TT"
pretty TOP = showString "_||_"
pretty (NEG r) = showString "~" . pretty r
pretty (MEET r s) = parens $ pretty r . showString " /\\ " . pretty s
pretty (JOIN r s) = parens $ pretty r . showString " \\/ " . pretty s
pretty ID = showString "id"
pretty (CONV r) = pretty r . showString "*"
pretty (COMP _ r s) = parens $ pretty r . showString " . " . pretty s
pretty (SPLIT r s) = 
  showString "<" . pretty r . showString ", " . pretty s . showString ">"
pretty (ORD o) = pretty o
pretty (FUN f) = pretty f
pretty (LEFTSEC _ f s) = 
  parens $ showString "<" . pretty s . showString ">" . pretty f 
pretty (RIGHTSEC _ f s) =
  parens $ pretty f . showString "<" . pretty s . showString ">"
pretty (APPLY _ r v) = 
  parens $ pretty r . space . pretty v
pretty (DEF nm t) = showString nm
pretty (Var nm) = showString nm
pretty (PROD r s) = parens $ pretty r . showString " >< " . pretty s
pretty (EITHER r s) = parens $ pretty r . showString " -|- " . pretty s
pretty (MAYBE r) = showString ""
pretty (LIST r) = showString ""
pretty (SET r) =showString ""
pretty (MAP r) =showString ""
pretty (REYNOLDS r s) =showString ""
pretty FId = showString "id"
pretty (FComp _ f g) = parens $ pretty f . showString " . " . pretty g
pretty OId = showString ""
pretty (OComp o1 o2) = showString ""
pretty (OConv o) = showString ""
pretty (OProd o) = showString ""
pretty (OJoin o) = showString ""
pretty (OMeet o) = showString ""
pretty (OMax o) = showString ""
pretty (OMin o) = showString ""
pretty (GDef nm f g fo go) = showString ""
pretty GId = showString ""
pretty (GComp _ g1 g2) = showString ""
pretty (GConv g) = showString ""
pretty (GLAdj g) = showString ""
pretty (GUAdj g) = showString ""
pretty (GLOrd _ g) = showString ""
pretty (GUOrd _ g) = showString ""

-------------------------------------------------------------------------------
