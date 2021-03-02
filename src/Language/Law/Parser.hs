
{-# OPTIONS_GHC -Wall #-}
 
-------------------------------------------------------------------------------

{- |
Module      :  Language.Law.Parser
Description :  
Copyright   :  (c) Paulo Silva
License     :  LGPL
  
Maintainer  :  paufil@di.uminho.pt
Stability   :  experimental
Portability :  portable
 
<description of the module>
-}

-------------------------------------------------------------------------------

module Language.Law.Parser (
  parser,
  parseLaw
 ) where

import Control.Monad
import Language.Law.SyntaxADT
import qualified Language.R.Parser as R
--import Language.R.SyntaxADT
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as P

-------------------------------------------------------------------------------

type LawParser = Parser LawS

-------------------------------------------------------------------------------

reservNames :: [String]
reservNames = ["EQUIV", "IMPL"]

lexer :: P.TokenParser st
lexer = P.makeTokenParser $ emptyDef { P.reservedNames = reservNames }

-------------------------------------------------------------------------------

reserved :: String -> CharParser st ()
reserved = P.reserved lexer

whiteSpace :: CharParser st ()
whiteSpace = P.whiteSpace lexer

identifier :: CharParser st String
identifier = P.identifier lexer

-------------------------------------------------------------------------------

parser :: String -> Either ParseError LawS
parser = parse mainLawParser "" 

-------------------------------------------------------------------------------

mainLawParser :: LawParser 
mainLawParser = do
  whiteSpace
  l <- parseLaw
  eof
  return l

-------------------------------------------------------------------------------

parseLaw :: LawParser
parseLaw = 
  parseEquiv <|>
  parseImpl 

-------------------------------------------------------------------------------

parseEquiv :: LawParser
parseEquiv = do
  p <- getPosition
  reserved "EQUIV"
  ident <- identifier
  r1 <- R.parseR
  r2 <- R.parseR
  return $ EquivS p ident r1 r2
  
-------------------------------------------------------------------------------

parseImpl :: LawParser
parseImpl = do
  p <- getPosition
  reserved "IMPL"
  ident <- identifier
  r1 <- R.parseR
  r2 <- R.parseR
  return $ ImplS p ident r1 r2

-------------------------------------------------------------------------------




