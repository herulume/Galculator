

module Language.Law.SyntaxADT (
  LawS(..),
  getName
 )where

import Language.R.SyntaxADT
import Text.ParserCombinators.Parsec.Pos

data LawS = 
    EquivS SourcePos String S S
  | ImplS SourcePos String S S
  deriving (Eq, Show)

getName :: LawS -> String
getName (EquivS _ n _ _) = n
getName (ImplS _ n _ _) = n
