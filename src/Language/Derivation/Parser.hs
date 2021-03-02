
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}
 
-------------------------------------------------------------------------------
 
{- |
Module      :  Language.Derivation.Parser
Description :  
Copyright   :  (c) Paulo Silva
License     :  LGPL
 
Maintainer  :  paufil@di.uminho.pt
Stability   :  experimental
Portability :  portable
 
-}

-------------------------------------------------------------------------------

module Language.Derivation.Parser (
  parser,
  parseDeriv
 ) where

import Control.GalcError
import Control.Monad.Error
import Language.Derivation.Syntax
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as P

-------------------------------------------------------------------------------

type DerivParser = Parser Derivation

-------------------------------------------------------------------------------

reservNames :: [String]
reservNames = derivations

-------------------------------------------------------------------------------

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

parser :: MonadError GalcError m => String -> m Derivation
parser = either2error (ParsingError . show) . parse mainDerivParser ""

-------------------------------------------------------------------------------

mainDerivParser :: DerivParser
mainDerivParser = do
  whiteSpace
  t <- parseDeriv
  eof
  return t

-------------------------------------------------------------------------------

parseDeriv :: DerivParser
parseDeriv = 
  parseInv  <|>
  parseShunt <|>
  parseDistrLow <|>
  parseDistrUp <|>
  parseMonotUp <|>
  parseMonotLow <|>
  parseTopPreserv <|>
  parseBotPreserv <|>
  parseCancUp <|>
  parseCancLow <|>
  parseFree <|>
  parseApply 

-------------------------------------------------------------------------------

parseInv :: DerivParser
parseInv = do 
  reserved "inv"
  drv <- parseDeriv
  return $ Inv drv

-------------------------------------------------------------------------------

parseShunt :: DerivParser
parseShunt = do 
  reserved "shunt"
  ident <- identifier
  return $ Shunt ident

-------------------------------------------------------------------------------

parseDistrLow :: DerivParser
parseDistrLow = do 
  reserved "distr_low"
  ident <- identifier
  return $ DistrLow ident

-------------------------------------------------------------------------------

parseDistrUp :: DerivParser
parseDistrUp = do 
  reserved "distr_up"
  ident <- identifier
  return $ DistrUp ident

-------------------------------------------------------------------------------

parseMonotUp :: DerivParser
parseMonotUp = do 
  reserved "monot_up"
  ident <- identifier
  return $ MonotUp ident

-------------------------------------------------------------------------------

parseMonotLow :: DerivParser
parseMonotLow = do 
  reserved "monot_low"
  ident <- identifier
  return $ MonotLow ident

-------------------------------------------------------------------------------

parseTopPreserv :: DerivParser
parseTopPreserv = do 
  reserved "top_preserving"
  ident <- identifier
  return $ TopPreserv ident

-------------------------------------------------------------------------------

parseBotPreserv :: DerivParser
parseBotPreserv = do 
  reserved "bot_preserving"
  ident <- identifier
  return $ BotPreserv ident

-------------------------------------------------------------------------------

parseCancUp :: DerivParser
parseCancUp = do 
  reserved "canc_up"
  ident <- identifier
  return $ CancUp ident

-------------------------------------------------------------------------------

parseCancLow :: DerivParser
parseCancLow = do 
  reserved "canc_low"
  ident <- identifier
  return $ CancLow ident

-------------------------------------------------------------------------------

parseFree :: DerivParser
parseFree = do 
  reserved "free"
  ident <- identifier
  return $ Free ident

-------------------------------------------------------------------------------

parseApply  :: DerivParser
parseApply  = do 
  reserved "apply"
  ident <- identifier
  return $ Apply ident

-------------------------------------------------------------------------------


