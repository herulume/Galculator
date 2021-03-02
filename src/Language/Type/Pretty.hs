
{-# OPTIONS_GHC -Wall #-}

-------------------------------------------------------------------------------

{- |
Module      :  Language.Type.Pretty
Description :  Pretty-printer of the type representation.
Copyright   :  (c) Paulo Silva
License     :  LGPL
 
Maintainer  :  paufil@di.uminho.pt
Stability   :  experimental
Portability :  portable

-}


-------------------------------------------------------------------------------

module Language.Type.Pretty (
  showType
 ) where

import {-# SOURCE #-} Language.Type.Syntax

showType :: Type t -> ShowS
showType (TVar a) =
  parens $
    showString "TVar " .
    showString a

showType One      = showString "One"
showType Bool     = showString "Bool"
showType Char     = showString "Char"
showType String   = showString "String"
showType Int      = showString "Int"
showType Float    = showString "Float"

showType (Prod a b) =
  parens $
    showString "Prod " .
    showType a .
    space .
    showType b

showType (Either a b) =
  parens $
    showString "Either " .
    showType a .
    space .
    showType b

showType (Maybe a) =
  parens $
    showString "Maybe " .
    showType a

showType (List a) =
  parens $
    showString "List " .
    showType a

showType (Set a) =
  parens $
    showString "Set " .
    showType a

showType (Map a b) =
  parens $
    showString "Map " .
    showType a .
    space .
    showType b

showType (Fun a b) =
  parens $
    showString "Fun " .
    showType a .
    space .
    showType b

showType (Rel a b) =
  parens $
    showString "Rel " .
    showType a .
    space .
    showType b

showType (Ord a) =
  parens $
    showString "Ord " .
    showType a

showType (GC a b) =
  parens $
    showString "GC " .
    showType a .
    space .
    showType b

-------------------------------------------------------------------------------

parens :: ShowS -> String -> String
parens = showParen True 

-------------------------------------------------------------------------------

space :: ShowS
space = showChar ' '

-------------------------------------------------------------------------------

