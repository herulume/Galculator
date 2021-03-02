
{-# OPTIONS_GHC -Wall #-}

-------------------------------------------------------------------------------

{- |
Module      :  Language.R.SyntaxADT
Description :  Representation of the expressions used by Galculator, using 
               regular Abstract Data Types.
Copyright   :  (c) Paulo Silva
License     :  LGPL

Maintainer  :  paufil@di.uminho.pt
Stability   :  experimental
Portability :  portable

-}

-------------------------------------------------------------------------------

module Language.R.SyntaxADT (
  S(..) 
 ) where

import Language.R.Syntax
import Language.Type.Syntax
import Text.ParserCombinators.Parsec.Pos

-------------------------------------------------------------------------------

data S = 
    RefS SourcePos String
  | RefExtS SourcePos RType
  | BotS SourcePos
  | TopS SourcePos 
  | NegS SourcePos S
  | MeetS SourcePos S S
  | JoinS SourcePos S S
  | IdS SourcePos
  | ConvS SourcePos S
  | CompS SourcePos S S
  | SplitS SourcePos S S
  | OrdS SourcePos S
  | FunS SourcePos S
  | LeftsecS SourcePos S S
  | RightsecS SourcePos S S
  | ApplyS SourcePos S S
  | DefS SourcePos String TypeBox
  | VarS SourcePos String
  | ProdS SourcePos S S
  | EitherS SourcePos S S
  | MaybeS SourcePos S
  | ListS SourcePos S
  | SetS SourcePos S
  | MapS SourcePos S
  | ReynoldsS SourcePos S S
  | FIdS SourcePos
  | FCompS SourcePos S S
  | OIdS SourcePos
  | OCompS SourcePos S S
  | OConvS SourcePos S
  | OProdS SourcePos S
  | OJoinS SourcePos S
  | OMeetS SourcePos S
  | OMaxS SourcePos S
  | OMinS SourcePos S
  | GDefS SourcePos String S S S S
  | GIdS SourcePos
  | GCompS SourcePos S S
  | GConvS SourcePos S
  | GLAdjS SourcePos S
  | GUAdjS SourcePos S
  | GLOrdS SourcePos S
  | GUOrdS SourcePos S
  deriving (Eq, Show)

-------------------------------------------------------------------------------

