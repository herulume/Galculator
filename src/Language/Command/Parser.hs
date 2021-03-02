
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}
 
-------------------------------------------------------------------------------
 
{- |
Module      :  Language.Command.Parser
Description :  Command line parser.
Copyright   :  (c) Paulo Silva
License     :  LGPL
 
Maintainer  :  paufil@di.uminho.pt
Stability   :  experimental
Portability :  portable
 
-}

-------------------------------------------------------------------------------

module Language.Command.Parser (
  parser
 ) where

import Control.GalcError
import Control.Monad.Error
import qualified Language.Step.Parser as S
import Language.Command.Syntax
import qualified Language.Derivation.Parser as D
import qualified Language.Law.Parser as L
import qualified Language.R.Parser as R
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as P

-------------------------------------------------------------------------------

type CmdParser = Parser Command

-------------------------------------------------------------------------------

reservNames :: [String]
reservNames = commands

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

parser :: MonadError GalcError m => String -> m Command
parser = either2error (ParsingError . show) . parse mainCmdParser ""

-------------------------------------------------------------------------------

mainCmdParser :: CmdParser 
mainCmdParser = do
  whiteSpace
  t <- parseCmd
  eof
  return t

-------------------------------------------------------------------------------

parseCmd :: CmdParser 
parseCmd = 
  parseAbort       <|>
  parseAssume      <|>
  parseAuto        <|>
  parseBrowse      <|>
  parseCombin      <|>
  parseDefine      <|>
  parseDerive      <|>
  parseHelp        <|>
  parseHint        <|>
  parseInfo        <|>
  parseLoad        <|>
  parseModules     <|>
  parseProve       <|>
  parseQuit        <|> 
  parseReload      <|>
  parseRestart     <|>
  parseRules       <|>
  parseSave        <|>
  parseShow        <|>
  parseType        <|>
  parseUndo        <|>
  parseUnload

-------------------------------------------------------------------------------

parseAbort :: CmdParser
parseAbort = do
  reserved "abort"
  return Abort

-------------------------------------------------------------------------------

parseAssume :: CmdParser
parseAssume = do
  reserved "assume"
  l <- L.parseLaw
  return $ Assume l

-------------------------------------------------------------------------------

parseAuto :: CmdParser
parseAuto = do
  reserved "auto"
  return Auto

-------------------------------------------------------------------------------

parseBrowse :: CmdParser 
parseBrowse = do
  reserved "browse"
  ident <- identifier
  return $ Browse ident

-------------------------------------------------------------------------------

parseCombin :: CmdParser 
parseCombin = do
  comb <- S.parseStep
  return $ Comb comb

-------------------------------------------------------------------------------

parseDefine :: CmdParser
parseDefine = do
  reserved "define"
  r <- R.parseR
  return $ Define r

-------------------------------------------------------------------------------

parseDerive :: CmdParser
parseDerive = do
  reserved "derive"
  drv <- D.parseDeriv
  return $ Derive drv

-------------------------------------------------------------------------------

parseHelp :: CmdParser 
parseHelp = do
  reserved "help"
  return Help

-------------------------------------------------------------------------------

parseHint :: CmdParser
parseHint = do
  reserved "hint"
  return Hint

-------------------------------------------------------------------------------

parseInfo :: CmdParser
parseInfo = do
  reserved "info"
  ident <- identifier
  return $ Info ident

-------------------------------------------------------------------------------

parseLoad :: CmdParser 
parseLoad = do
  reserved "load"
  ident <- identifier
  return $ Load ident

-------------------------------------------------------------------------------

parseModules :: CmdParser 
parseModules = do
  reserved "modules"
  return Modules

-------------------------------------------------------------------------------

parseProve :: CmdParser
parseProve = do
  reserved "prove"
  l <- L.parseLaw
  return $ Prove l

-------------------------------------------------------------------------------

parseQuit :: CmdParser 
parseQuit = do
  reserved "quit"
  return Quit

-------------------------------------------------------------------------------

parseReload :: CmdParser
parseReload = do
  reserved "reload"
  return Reload

-------------------------------------------------------------------------------

parseRestart :: CmdParser
parseRestart = do
  reserved "restart"
  return Restart

-------------------------------------------------------------------------------

parseRules :: CmdParser
parseRules = do
  reserved "rules"
  return Rules

-------------------------------------------------------------------------------

parseSave :: CmdParser
parseSave = do
  reserved "save"
  return Save

-------------------------------------------------------------------------------

parseShow :: CmdParser
parseShow = do
  reserved "show"
  return Show

-------------------------------------------------------------------------------

parseType :: CmdParser 
parseType = do
  reserved "type"
  expr <- R.parseR 
  return $ Type expr

-------------------------------------------------------------------------------

parseUndo :: CmdParser 
parseUndo = do
  reserved "undo"
  return $ Undo Nothing

-------------------------------------------------------------------------------

parseUnload :: CmdParser
parseUnload = do
  reserved "unload"
  ident <- identifier
  return $ Unload ident

-------------------------------------------------------------------------------
