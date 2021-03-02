
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}

-------------------------------------------------------------------------------

{- |
Module      :  Galculator.Interpreter
Description :  Command line interpreter of Galculator.
Copyright   :  (c) Paulo Silva
License     :  LGPL

Maintainer  :  paufil@di.uminho.pt
Stability   :  experimental
Portability :  portable

-}

-------------------------------------------------------------------------------

module Galculator.Interpreter (
 interactiveUI
 ) where

import Control.GalcError
import Control.Monad.Error
import Control.Monad.Fresh
import Control.Monad.Trans
import Data.Char( isSpace )
import Data.IORef
import Data.List
import Galculator.RunCommand
import Galculator.State
import Language.Command.Parser
import Language.Command.Syntax
import System.Console.Readline 


-------------------------------------------------------------------------------

-- | Sets the interactive environment: initializes the readline library, puts
--   the welcome banner and lanches the interactive loop with the initial state.
--   At exit resets the readline library.
interactiveUI :: IO ()
interactiveUI = do
  initialize
  setAttemptedCompletionFunction $ Just completeCommand 
  putStrLn galculatorBanner
  startGalculator interactiveLoop emptyState
  putStrLn "Leaving Galculator"
  resetTerminal Nothing
  return ()

-------------------------------------------------------------------------------

-- | Initializes a new session with a given state.
startGalculator :: FreshT [String] GalcState a -> GalcSt -> IO a
startGalculator g state = 
  aux (evalFreshT g (map show ([1..]::[Integer]))) state
  where aux :: GalcState a -> GalcSt -> IO a
        aux g' state' = do
          ref <- newIORef state'
          unGalcState g' ref

-------------------------------------------------------------------------------

prompt :: String
prompt = "Galculator> "

-------------------------------------------------------------------------------

interactiveLoop :: FreshT [String] GalcState ()
interactiveLoop = runErrorT (interactiveLoop') >> return ()

-------------------------------------------------------------------------------

interactiveLoop' :: GalcStateT ()
interactiveLoop' = do
  cmd <- liftIO . readline $ prompt
  exit <- maybe (return False) aux cmd
  if exit then return () else interactiveLoop'
  where
    aux c = if null . removeSpaces $ c
            then return False
            else (do
              liftIO . addHistory $ c
              runCommand =<< parser c) `catchError` showError

-------------------------------------------------------------------------------

showError :: GalcError -> GalcStateT Bool
showError = keepGoing . liftIO . putStrLn . (++) "***Galculator error:\n" . show

-------------------------------------------------------------------------------

completeCommand :: String -> Int -> Int -> IO (Maybe (String, [String]))
completeCommand str _ _ = do
  let lst = foldr (\c r -> if str `isPrefixOf` c then c:r else r) [] commands
  case lst of
    [] -> return Nothing
    [x] -> return $ Just (x,[])
    xs -> return $ Just (getCommonPrefix xs, xs)

-------------------------------------------------------------------------------
-- Stollen from GHCi
getCommonPrefix :: [String] -> String
getCommonPrefix [] = ""
getCommonPrefix (str:ss) = foldl common str ss
  where common _ "" = ""
        common "" _ = ""
        common (c:cs) (d:ds) 
          | c == d = c : common cs ds
          | otherwise = ""

-------------------------------------------------------------------------------
-- Stollen from GHC Utils
removeSpaces :: String -> String
removeSpaces = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-------------------------------------------------------------------------------

galculatorBanner :: String
galculatorBanner = 
 "   ___    __    _      ___  _   _  _         __   _____  __   ___ \n" ++
 "  / _ \\  /  \\  | |    / __|| | | || |       /  \\ |_   _|/   \\|    \\\n" ++
 " / /_\\/ / /\\ \\ | |   | |   | | | || |      / /\\ \\  | |  | | ||    /\n" ++
 "/ /_\\\\ / ___  \\| |___| |__ | |_| || |___  / ___  \\ | |  | | || |\\ \\\n" ++
 "\\____//_/   \\_/ \\____|\\___|\\____/  \\____|/_/   \\_/ |_|  \\___/|_|  \\_\\\n\n" ++
 " Paulo Silva (paufil@di.uminho.pt)\n" ++ 
 " Universidade do Minho, Braga, Portugal\n"

-------------------------------------------------------------------------------
