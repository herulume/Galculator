
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}
 
-------------------------------------------------------------------------------

{- |
Module      :  Language.Module.Parser
Description :  
Copyright   :  (c) Paulo Silva
License     :  LGPL
  
Maintainer  :  paufil@di.uminho.pt
Stability   :  experimental
Portability :  portable
 
<description of the module>
-}

-------------------------------------------------------------------------------

module Language.Module.Parser (
  parser
 ) where

import Control.GalcError
import Control.Monad.Error
import Language.Law.Parser hiding (parser)
import Language.Law.SyntaxADT
import qualified Language.Law.SyntaxADT as L
import Language.Module.SyntaxADT
import Language.R.Parser hiding (parser)
import Language.R.SyntaxADT
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as P

-------------------------------------------------------------------------------

type ModuleParser = Parser ModuleS

-------------------------------------------------------------------------------

reservNames :: [String]
reservNames = ["module"]

-------------------------------------------------------------------------------

lexer :: P.TokenParser st
lexer = P.makeTokenParser $ haskellStyle { P.reservedNames = reservNames }

-------------------------------------------------------------------------------

reserved :: String -> CharParser st ()
reserved = P.reserved lexer

whiteSpace :: CharParser st ()
whiteSpace = P.whiteSpace lexer

{-
parens :: CharParser st Module -> CharParser st Module
parens = P.parens lexer
-}

identifier :: CharParser st String
identifier = P.identifier lexer

semi :: CharParser st String
semi = P.semi lexer

-------------------------------------------------------------------------------

parser :: MonadError GalcError m => String -> m ModuleS
parser = either2error (ParsingError . show) . parse mainModuleParser ""
 
-------------------------------------------------------------------------------

mainModuleParser :: ModuleParser
mainModuleParser = do
  whiteSpace 
  m <- parseModule
  eof
  return m

-------------------------------------------------------------------------------

data Union = L LawS | G S | D S

parseModule :: ModuleParser
parseModule = do
  reserved "module"
  ident <- identifier
  lst <- ((do l <- parseLaw
              return $ L l)
          <|>
          (do g <- parseGDef
              return $ G g)
          <|>
          (do d <- parseDEF
              return $ D d)) `sepEndBy` semi
  let (lws,gcs',defs) = union2List lst
  return $ ModuleS { nameS = ident,
                     lawsS = lws,
                     gcsS = gcs',
                     definitionsS = defs }

-------------------------------------------------------------------------------

union2List :: [Union] -> ([LawS],[S],[S])
union2List = foldl aux ([],[],[])
  where aux (law,gc,def) (L law') = (law':law, gc, def)
        aux (law,gc,def) (G gc') = (law, gc':gc, def)
        aux (law,gc,def) (D def') = (law, gc, def':def)

-------------------------------------------------------------------------------

