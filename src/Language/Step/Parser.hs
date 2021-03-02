
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}
 
-------------------------------------------------------------------------------
 
{- |
Module      :  Language.Step.Parser
Description :  Proof step parser.
Copyright   :  (c) Paulo Silva
License     :  LGPL
 
Maintainer  :  paufil@di.uminho.pt
Stability   :  experimental
Portability :  portable
 
-}

-------------------------------------------------------------------------------

module Language.Step.Parser (
  parser,
  parseStep
 ) where

import Control.GalcError
import Control.Monad.Error
import qualified Language.Combinator.Parser as C
import qualified Language.R.Parser as R
import Language.Step.Syntax
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as P

-------------------------------------------------------------------------------

type StepParser = Parser Step

-------------------------------------------------------------------------------

lexer :: P.TokenParser st
lexer = P.makeTokenParser $ emptyDef { P.reservedNames = steps }

-------------------------------------------------------------------------------

reserved :: String -> CharParser st ()
reserved = P.reserved lexer

whiteSpace :: CharParser st ()
whiteSpace = P.whiteSpace lexer

parens :: CharParser st Step  -> CharParser st Step
parens = P.parens lexer

-------------------------------------------------------------------------------

parser :: MonadError GalcError m => String -> m Step
parser = either2error (ParsingError . show) . parse mainStepParser ""

-------------------------------------------------------------------------------

mainStepParser :: StepParser
mainStepParser = do
  whiteSpace
  t <- parseStep
  eof
  return t

-------------------------------------------------------------------------------

parseStep :: StepParser
parseStep = 
  parens parseStep <|>
  parseCombin      <|>
  parseIndirectUp  <|>
  parseIndirectLow <|>
  parseIndirectEnd <|>
  parseLeft        <|>
  parseQed         <|>
  parseRight       <|>
  parseSeqc

-------------------------------------------------------------------------------

parseCombin :: StepParser
parseCombin = do
  comb <- C.parseComb
  return $ Comb comb

-------------------------------------------------------------------------------

parseIndirectUp :: StepParser
parseIndirectUp = do
  try (do reserved "indirect"
          reserved "up")
  r <- R.parseR
  return $ Indirect (Right r)

-------------------------------------------------------------------------------

parseIndirectLow :: StepParser
parseIndirectLow = do
  try (do reserved "indirect"
          reserved "low")
  r <- R.parseR
  return $ Indirect (Left r)

-------------------------------------------------------------------------------

parseIndirectEnd :: StepParser
parseIndirectEnd = do
  try (do reserved "indirect"
          reserved "end")
  return IndirectEnd

-------------------------------------------------------------------------------

parseLeft :: StepParser
parseLeft = do
  reserved "left"
  return LeftP

-------------------------------------------------------------------------------

parseQed :: StepParser
parseQed = do
  reserved "qed"
  return Qed

-------------------------------------------------------------------------------

parseRight :: StepParser
parseRight = do
  reserved "right"
  return RightP

-------------------------------------------------------------------------------

parseSeqc :: StepParser
parseSeqc = do
  reserved "seqc"
  s1 <- parseStep
  s2 <- parseStep
  return $ SeqC s1 s2

-------------------------------------------------------------------------------
