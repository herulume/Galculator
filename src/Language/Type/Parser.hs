
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}
 
-------------------------------------------------------------------------------
 
{- |
Module      :  Language.Type.Parser
Description :  Parser of the type representation.
Copyright   :  (c) Paulo Silva
License     :  LGPL
 
Maintainer  :  paufil@di.uminho.pt
Stability   :  experimental
Portability :  portable

-}

-------------------------------------------------------------------------------

module Language.Type.Parser (
  parser,
  parseType
 ) where

import Control.GalcError
import Control.Monad.Error
import Data.Existential
import Language.Type.Syntax
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as P

-------------------------------------------------------------------------------

type TypeParser = Parser TypeBox 

-------------------------------------------------------------------------------

reservNames :: [String]
reservNames = [
  "TVar", "One", "Bool", "Char", "String", "Int", "Float", "Prod", 
  "Either", "Maybe", "List", "Set", "Map", "Fun", "Rel", "Ord", "GC"
 ]

-------------------------------------------------------------------------------

lexer :: P.TokenParser st
lexer = P.makeTokenParser $ emptyDef { P.reservedNames = reservNames }

-------------------------------------------------------------------------------

reserved :: String -> CharParser st ()
reserved = P.reserved lexer

whiteSpace :: CharParser st ()
whiteSpace = P.whiteSpace lexer

parens :: CharParser st TypeBox -> CharParser st TypeBox
parens = P.parens lexer

identifier :: CharParser st String
identifier = P.identifier lexer

-------------------------------------------------------------------------------

parser :: MonadError GalcError m => String -> m TypeBox
parser = either2error (ParsingError . show) . parse mainTypeParser ""

-------------------------------------------------------------------------------

mainTypeParser :: TypeParser 
mainTypeParser = do
  whiteSpace
  t <- parseType
  eof
  return t

-------------------------------------------------------------------------------

parseType :: TypeParser 
parseType = 
  parens parseType <|>
  parseTVar <|>
  parseOne <|> 
  parseBool <|>
  parseChar <|>
  parseString <|>
  parseInt <|>
  parseFloat <|>
  parseProd <|>
  parseEither <|>
  parseMaybe <|>
  parseList <|>
  parseSet <|>
  parseMap <|>
  parseFun <|>
  parseRel <|>
  parseOrd <|>
  parseGC

-------------------------------------------------------------------------------

parseTVar :: TypeParser
parseTVar = do
  reserved "TVar"
  tid <- identifier
  return $ Hide $ TVar tid

-------------------------------------------------------------------------------

parseOne :: TypeParser 
parseOne = do
  reserved "One"
  return $ Hide One

-------------------------------------------------------------------------------

parseBool :: TypeParser 
parseBool = do
  reserved "Bool"
  return $ Hide Bool

-------------------------------------------------------------------------------

parseChar :: TypeParser 
parseChar = do
  reserved "Char"
  return $ Hide Char

-------------------------------------------------------------------------------

parseString :: TypeParser 
parseString = do
  reserved "String"
  return $ Hide String

-------------------------------------------------------------------------------

parseInt :: TypeParser 
parseInt = do
  reserved "Int"
  return $ Hide Int

-------------------------------------------------------------------------------

parseFloat :: TypeParser 
parseFloat = do
  reserved "Float"
  return $ Hide Float

-------------------------------------------------------------------------------

parseProd :: TypeParser 
parseProd = do
  reserved "Prod"
  Hide t1 <- parseType
  Hide t2 <- parseType
  return $ Hide (Prod t1 t2)

-------------------------------------------------------------------------------

parseEither :: TypeParser 
parseEither = do
  reserved "Either"
  Hide t1 <- parseType
  Hide t2 <- parseType
  return $ Hide (Either t1 t2)

-------------------------------------------------------------------------------

parseMaybe :: TypeParser 
parseMaybe = do
  reserved "Maybe"
  Hide t1 <- parseType
  return $ Hide (Maybe t1)

-------------------------------------------------------------------------------

parseList :: TypeParser 
parseList = do
  reserved "List"
  Hide t1 <- parseType
  return $ Hide (List t1)

-------------------------------------------------------------------------------

parseSet :: TypeParser 
parseSet = do
  reserved "Set"
  Hide t1 <- parseType
  return $ Hide (Set t1)

-------------------------------------------------------------------------------

parseMap :: TypeParser 
parseMap = do
  reserved "Map"
  Hide t1 <- parseType
  Hide t2 <- parseType
  return $ Hide (Map t1 t2)

-------------------------------------------------------------------------------

parseFun :: TypeParser 
parseFun = do
  reserved "Fun"
  Hide t1 <- parseType
  Hide t2 <- parseType
  return $ Hide (Fun t1 t2)

-------------------------------------------------------------------------------

parseRel :: TypeParser 
parseRel = do
  reserved "Rel"
  Hide t1 <- parseType
  Hide t2 <- parseType
  return $ Hide (Rel t1 t2)

-------------------------------------------------------------------------------

parseOrd :: TypeParser 
parseOrd = do
  reserved "Ord"
  Hide t1 <- parseType
  return $ Hide (Ord t1)

-------------------------------------------------------------------------------

parseGC :: TypeParser 
parseGC = do
  reserved "GC"
  Hide t1 <- parseType
  Hide t2 <- parseType
  return $ Hide (GC t1 t2)

-------------------------------------------------------------------------------
