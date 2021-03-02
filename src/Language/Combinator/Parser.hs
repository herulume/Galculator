
{-# LANGUAGE TypeOperators, FlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}
 
-------------------------------------------------------------------------------
 
{- |
Module      :  Language.Combinator.Parser
Description :  
Copyright   :  (c) Paulo Silva
License     :  LGPL
 
Maintainer  :  paufil@di.uminho.pt
Stability   :  experimental
Portability :  portable
 
<description of the module>
-}

-------------------------------------------------------------------------------

module Language.Combinator.Parser (
  parser,
  parseComb
 ) where

import Control.GalcError
import Control.Monad.Error
import Language.Combinator.Syntax
import qualified Language.Derivation.Parser as D
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as P

-------------------------------------------------------------------------------

type CombParser = Parser Combinator 

-------------------------------------------------------------------------------

reservNames :: [String]
reservNames = combinators

-------------------------------------------------------------------------------

lexer :: P.TokenParser st
lexer = P.makeTokenParser $ emptyDef { P.reservedNames = reservNames }

-------------------------------------------------------------------------------

reserved :: String -> CharParser st ()
reserved = P.reserved lexer

whiteSpace :: CharParser st ()
whiteSpace = P.whiteSpace lexer

parens :: CharParser st Combinator -> CharParser st Combinator
parens = P.parens lexer

-------------------------------------------------------------------------------

parser :: MonadError GalcError m => String -> m Combinator
parser = either2error (ParsingError . show) . parse mainCombParser ""

-------------------------------------------------------------------------------

mainCombParser :: CombParser
mainCombParser = do
  whiteSpace
  t <- parseComb
  eof
  return t

-------------------------------------------------------------------------------

parseComb :: CombParser
parseComb = 
  parens parseComb <|>
  parseNop <|>
  parseFail <|>
  parseSeq <|>
  parseChoice <|>
  parseLChoice <|>
  parseMany <|>
  parseMany1 <|>
  parseTry <|>
  parseOnce <|>
  parseEverywhere <|>
  parseEverywhere' <|>
  parseInnermost <|>
  parseAll <|>
  parseOne <|>
  parseRule

-------------------------------------------------------------------------------

parseNop :: CombParser
parseNop = do
  reserved "nop"
  return Nop

-------------------------------------------------------------------------------

parseFail :: CombParser
parseFail = do
  reserved "fail"
  return Fail

-------------------------------------------------------------------------------

parseSeq :: CombParser
parseSeq = do
  reserved "seq"
  c1 <- parseComb
  c2 <- parseComb
  return $ Seq c1 c2

-------------------------------------------------------------------------------

parseChoice :: CombParser
parseChoice = do
  reserved "choice"
  c1 <- parseComb
  c2 <- parseComb
  return $ Choice c1 c2

-------------------------------------------------------------------------------

parseLChoice :: CombParser
parseLChoice = do
  reserved "lchoice"
  c1 <- parseComb
  c2 <- parseComb
  return $ LChoice c1 c2

-------------------------------------------------------------------------------

parseMany :: CombParser
parseMany = do
  reserved "many"
  c <- parseComb
  return $ Many c

-------------------------------------------------------------------------------

parseMany1 :: CombParser
parseMany1 = do
  reserved "many1"
  c <- parseComb
  return $ Many1 c

-------------------------------------------------------------------------------

parseTry :: CombParser
parseTry = do
  reserved "try"
  c <- parseComb
  return $ Try c

-------------------------------------------------------------------------------

parseOnce :: CombParser
parseOnce = do
  reserved "once"
  c <- parseComb
  return $ Once c

-------------------------------------------------------------------------------

parseEverywhere :: CombParser
parseEverywhere = do
  reserved "everywhere"
  c <- parseComb
  return $ Everywhere c

-------------------------------------------------------------------------------

parseEverywhere' :: CombParser
parseEverywhere' = do
  reserved "everywhere'"
  c <- parseComb
  return $ Everywhere' c

-------------------------------------------------------------------------------

parseInnermost :: CombParser
parseInnermost = do
  reserved "innermost"
  c <- parseComb
  return $ Innermost c

-------------------------------------------------------------------------------

parseAll :: CombParser
parseAll = do
  reserved "all"
  c <- parseComb
  return $ All c

-------------------------------------------------------------------------------

parseOne :: CombParser
parseOne = do
  reserved "one"
  c <- parseComb
  return $ One c

-------------------------------------------------------------------------------

parseRule :: CombParser
parseRule = do
  drv <- D.parseDeriv
  return $ Rule drv

-------------------------------------------------------------------------------
