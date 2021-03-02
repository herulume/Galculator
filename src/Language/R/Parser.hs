
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}
 
-------------------------------------------------------------------------------

{- |
Module      :  Language.R.Parser
Description :  Parser of the expression representation.
Copyright   :  (c) Paulo Silva
License     :  LGPL
  
Maintainer  :  paufil@di.uminho.pt
Stability   :  experimental
Portability :  portable
 
-}

-------------------------------------------------------------------------------

module Language.R.Parser (
  parser,
  parseR,
  parseGDef,
  parseDEF
 ) where

import Control.GalcError
import Control.Monad.Error
import Language.R.SyntaxADT
import qualified Language.Type.Parser as T
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as P

-------------------------------------------------------------------------------

type RParser = Parser S

-------------------------------------------------------------------------------

reservNames :: [String]
reservNames = [
  "BOT", "TOP", "NEG", "MEET", "JOIN", "ID", "CONV", "COMP", "SPLIT", 
  "ORD", "FUN", "LEFTSEC", "RIGHTSEC", "APPLY", "DEF", "Var",
  "PROD", "EITHER", "MAYBE", "LIST", "SET", "MAP", "FId", "FComp", 
  "OId", "OComp", "OConv", "OProd", "OJoin", "OMeet", "OMax", "OMin",
  "GDef", "GId", "GComp", "GConv", "GLAdj", "GUAdj", "GLOrd", "GUOrd"
 ]

-------------------------------------------------------------------------------

lexer :: P.TokenParser st
lexer = P.makeTokenParser $ emptyDef { P.reservedNames = reservNames }

-------------------------------------------------------------------------------

reserved :: String -> CharParser st ()
reserved = P.reserved lexer

whiteSpace :: CharParser st ()
whiteSpace = P.whiteSpace lexer

parens :: CharParser st S  -> CharParser st S
parens = P.parens lexer

identifier :: CharParser st String
identifier = P.identifier lexer

-------------------------------------------------------------------------------

parser :: MonadError GalcError m => String -> m S
parser = either2error (ParsingError . show) . parse mainRParser ""

-------------------------------------------------------------------------------

mainRParser :: RParser
mainRParser = do
  whiteSpace
  r <- parseR
  eof
  return r

-------------------------------------------------------------------------------

parseR :: RParser
parseR = 
  parens parseR <|>
  parseREF      <|>
  parseBOT      <|>
  parseTOP      <|>
  parseNEG      <|>
  parseMEET     <|>
  parseJOIN     <|>
  parseID       <|>
  parseCONV     <|>
  parseCOMP     <|>
  parseSPLIT    <|>
  parseORD      <|>
  parseFUN      <|>
  parseLEFTSEC  <|>
  parseRIGHTSEC <|>
  parseAPPLY    <|>
  parseDEF      <|>
  parseVar      <|>
  parsePROD     <|>
  parseEITHER   <|>
  parseMAYBE    <|>
  parseLIST     <|>
  parseSET      <|>
  parseMAP      <|>
  parseREYNOLDS <|>
  parseFId      <|>
  parseFComp    <|>
  parseOId      <|>
  parseOComp    <|>
  parseOConv    <|>
  parseOProd    <|>
  parseOJoin    <|>
  parseOMeet    <|>
  parseOMax     <|>
  parseOMin     <|>
  parseGDef     <|>
  parseGId      <|>
  parseGComp    <|>
  parseGConv    <|>
  parseGLAdj    <|>
  parseGUAdj    <|>
  parseGLOrd    <|>
  parseGUOrd 

-------------------------------------------------------------------------------

parseREF :: RParser
parseREF = do
  p <- getPosition
  reserved "REF"
  ident <- identifier
  return $ RefS p ident

-------------------------------------------------------------------------------

parseBOT :: RParser
parseBOT = do
  p <- getPosition
  reserved "BOT"
  return $ BotS p
  
-------------------------------------------------------------------------------

parseTOP :: RParser
parseTOP = do
  p <- getPosition
  reserved "TOP"
  return $ TopS p

-------------------------------------------------------------------------------

parseNEG :: RParser
parseNEG = do
  p <- getPosition
  reserved "NEG"
  r <- parseR
  return $ NegS p r

-------------------------------------------------------------------------------

parseMEET :: RParser
parseMEET = do
  p <- getPosition
  reserved "MEET"
  r1 <- parseR
  r2 <- parseR
  return $ MeetS p r1 r2

-------------------------------------------------------------------------------

parseJOIN :: RParser
parseJOIN = do
  p <- getPosition
  reserved "JOIN"
  r1 <- parseR
  r2 <- parseR
  return $ JoinS p r1 r2

-------------------------------------------------------------------------------

parseID :: RParser
parseID = do 
  p <- getPosition
  reserved "ID"
  return $ IdS p

-------------------------------------------------------------------------------

parseCONV :: RParser
parseCONV = do
  p <- getPosition
  reserved "CONV"
  r <- parseR
  return $ ConvS p r

-------------------------------------------------------------------------------

parseCOMP :: RParser
parseCOMP = do
  p <- getPosition
  reserved "COMP"
  r1 <- parseR
  r2 <- parseR
  return $ CompS p r1 r2

-------------------------------------------------------------------------------

parseSPLIT :: RParser
parseSPLIT = do
  p <- getPosition
  reserved "SPLIT"
  r1 <- parseR
  r2 <- parseR
  return $ SplitS p r1 r2

-------------------------------------------------------------------------------

parseORD :: RParser
parseORD = do
  p <- getPosition
  reserved "ORD"
  r <- parseR
  return $ OrdS p r

-------------------------------------------------------------------------------

parseFUN :: RParser
parseFUN = do
  p <- getPosition
  reserved "FUN"
  r <- parseR
  return $ FunS p r

-------------------------------------------------------------------------------

parseLEFTSEC :: RParser
parseLEFTSEC = do
  p <- getPosition
  reserved "LEFTSEC" 
  r1 <- parseR
  r2 <- parseR
  return $ LeftsecS p r1 r2

-------------------------------------------------------------------------------

parseRIGHTSEC :: RParser
parseRIGHTSEC = do
  p <- getPosition
  reserved "RIGHTSEC"
  r1 <- parseR
  r2 <- parseR
  return $ RightsecS p r1 r2

-------------------------------------------------------------------------------

parseAPPLY :: RParser
parseAPPLY = do
  p <- getPosition
  reserved "APPLY"
  r1 <- parseR
  r2 <- parseR
  return $ ApplyS p r1 r2

-------------------------------------------------------------------------------

parseDEF :: RParser
parseDEF = do
  p <- getPosition
  reserved "DEF"
  n <- identifier
  t <- T.parseType
  return $ DefS p n t

-------------------------------------------------------------------------------

parseVar :: RParser
parseVar = do
  p <- getPosition
  reserved "Var"
  n <- identifier
  return $ VarS p n

-------------------------------------------------------------------------------

parsePROD :: RParser
parsePROD = do
  p <- getPosition
  reserved "PROD"
  r1 <- parseR
  r2 <- parseR
  return $ ProdS p r1 r2

-------------------------------------------------------------------------------

parseEITHER :: RParser
parseEITHER = do
  p <- getPosition
  reserved "EITHER"
  r1 <- parseR
  r2 <- parseR
  return $ EitherS p r1 r2
  
-------------------------------------------------------------------------------

parseMAYBE :: RParser
parseMAYBE = do
  p <- getPosition
  reserved "MAYBE"
  r <- parseR
  return $ MaybeS p r

-------------------------------------------------------------------------------

parseLIST :: RParser
parseLIST = do
  p <- getPosition
  reserved "LIST"
  r <- parseR
  return $ ListS p r

-------------------------------------------------------------------------------

parseSET :: RParser
parseSET = do
  p <- getPosition
  reserved "SET"
  r <- parseR
  return $ SetS p r

-------------------------------------------------------------------------------

parseMAP :: RParser
parseMAP = do
  p <- getPosition
  reserved "MAP"
  r <- parseR
  return $ MapS p r

-------------------------------------------------------------------------------

parseREYNOLDS :: RParser
parseREYNOLDS = do
  p <- getPosition
  reserved "REYNOLDS"
  r1 <- parseR
  r2 <- parseR
  return $ ReynoldsS p r1 r2

-------------------------------------------------------------------------------

parseFId :: RParser
parseFId = do
  p <- getPosition
  reserved "FId"
  return $ FIdS p

-------------------------------------------------------------------------------

parseFComp :: RParser
parseFComp = do
  p <- getPosition
  reserved "FComp"
  r1 <- parseR
  r2 <- parseR
  return $ FCompS p r1 r2

-------------------------------------------------------------------------------

parseOId :: RParser
parseOId = do
  p <- getPosition
  reserved "OId"
  return $ OIdS p

-------------------------------------------------------------------------------

parseOComp :: RParser
parseOComp = do
  p <- getPosition
  reserved "OComp"
  r1 <- parseR
  r2 <- parseR
  return $ OCompS p r1 r2

-------------------------------------------------------------------------------

parseOConv :: RParser
parseOConv = do
  p <- getPosition
  reserved "OConv"
  r <- parseR
  return $ OConvS p r

-------------------------------------------------------------------------------

parseOProd :: RParser
parseOProd = do
  p <- getPosition
  reserved "OProd"
  r <- parseR
  return $ OProdS p r

-------------------------------------------------------------------------------

parseOJoin :: RParser
parseOJoin = do
  p <- getPosition
  reserved "OJoin"
  r <- parseR
  return $ OJoinS p r

-------------------------------------------------------------------------------

parseOMeet :: RParser
parseOMeet = do
  p <- getPosition
  reserved "OMeet"
  r <- parseR
  return $ OMeetS p r

-------------------------------------------------------------------------------

parseOMax :: RParser
parseOMax = do
  p <- getPosition
  reserved "OMax"
  r <- parseR
  return $ OMaxS p r

-------------------------------------------------------------------------------

parseOMin :: RParser
parseOMin = do
  p <- getPosition
  reserved "OMin"
  r <- parseR
  return $ OMinS p r

-------------------------------------------------------------------------------

parseGDef :: RParser
parseGDef = do
  p <- getPosition
  reserved "GDef"
  n <- identifier
  f1 <- parseR
  f2 <- parseR
  o1 <- parseR
  o2 <- parseR
  return $ GDefS p n f1 f2 o1 o2

-------------------------------------------------------------------------------

parseGId :: RParser
parseGId = do
  p <- getPosition
  reserved "GId"
  return $ GIdS p

-------------------------------------------------------------------------------

parseGComp :: RParser
parseGComp = do
  p <- getPosition
  reserved "GComp"
  r1 <- parseR
  r2 <- parseR
  return $ GCompS p r1 r2

-------------------------------------------------------------------------------

parseGConv :: RParser
parseGConv = do
  p <- getPosition
  reserved "GConv"
  r <- parseR
  return $ GConvS p r

-------------------------------------------------------------------------------

parseGLAdj :: RParser
parseGLAdj = do
  p <- getPosition
  reserved "GLAdj"
  r <- parseR
  return $ GLAdjS p r

-------------------------------------------------------------------------------

parseGUAdj :: RParser
parseGUAdj = do
  p <- getPosition
  reserved "GUAdj"
  r <- parseR
  return $ GUAdjS p r

-------------------------------------------------------------------------------

parseGLOrd :: RParser
parseGLOrd = do
  p <- getPosition
  reserved "GLOrd"
  r <- parseR
  return $ GLOrdS p r

-------------------------------------------------------------------------------

parseGUOrd :: RParser
parseGUOrd = do
  p <- getPosition
  reserved "GUOrd"
  r <- parseR
  return $ GUOrdS p r

-------------------------------------------------------------------------------

