
{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

-------------------------------------------------------------------------------

{- |
Module      :  Control.GalcError
Description :  Error messages of Galculator.
Copyright   :  (c) Paulo Silva
License     :  LGPL

Maintainer  :  paufil@di.uminho.pt
Stability   :  experimental
Portability :  portable

-}

-------------------------------------------------------------------------------

module Control.GalcError (
  GalcError(..),
  maybe2error,
  list2error,
  either2error,
 ) where

import Control.MonadOr
import Control.Monad.Error

-------------------------------------------------------------------------------

data GalcError = 
    UnknownError
  | NoValueError
  | ImpossibleError
  | CustomError String
  | ParsingError String
  | UnificationError (String, String)
  | CastingError (String, String)
  | InferenceError GalcError String
  | ReferenceError String
  | MultiDefError String
  | ModuleFileError String
  | ModuleRefError String
  | DerivationError
  | ModeError
  | NoProofError
  | ExistingRefError String
  | QedError String String
  | NoOrderSetError
  | RewriteError String
  | EqualityError 
  | NestedError GalcError
  | DecomposeError String String
 
-------------------------------------------------------------------------------

instance Show GalcError where
  show = showGalcError

-------------------------------------------------------------------------------

instance Error GalcError where
  noMsg = UnknownError
  strMsg = CustomError

-------------------------------------------------------------------------------

instance MonadError GalcError [] where
  throwError _ = []
  catchError [] f = f NoValueError
  catchError x@(_:_) _ = x

-------------------------------------------------------------------------------

instance MonadOr (Either GalcError) where
  morelse (Left _) b = b
  morelse a _ = a
  
-------------------------------------------------------------------------------

maybe2error :: MonadError GalcError m => GalcError -> Maybe a -> m a
maybe2error e Nothing  = throwError e
maybe2error _ (Just a) = return a

-------------------------------------------------------------------------------

list2error :: MonadError GalcError m => GalcError -> [a] -> m a
list2error e []    = throwError e
list2error _ (x:_) = return x

-------------------------------------------------------------------------------

either2error :: (Show e, MonadError GalcError m) 
             => (e -> GalcError) -> Either e a -> m a
either2error f (Left e)  = throwError $ f e
either2error _ (Right v) = return v

-------------------------------------------------------------------------------

showGalcError :: GalcError -> String
showGalcError UnknownError = 
  "Unknown error!"
showGalcError NoValueError = 
  "No values found"
showGalcError ImpossibleError = 
  "The impossible has happened!"
showGalcError (CustomError err) = err
showGalcError (ParsingError err) = 
  "Parsing error:\n" ++ err
showGalcError (UnificationError (s1, s2)) = 
  "Unification error\nCannot unify " ++ s1 ++ " and " ++ s2
showGalcError (CastingError (s1, s2)) = 
  "Cannot cast " ++ s1 ++ " to " ++ s2
showGalcError (InferenceError err s) = 
  "Error in type inference:\n" ++ showGalcError err ++ "\n" ++ s
showGalcError (ReferenceError err) = 
  "Invalid reference: " ++ err
showGalcError (MultiDefError err) =
  "Multiple definitions: " ++ err
showGalcError (ModuleFileError err) = 
  "Error opening file: " ++ err ++ ".gal"
showGalcError DerivationError =
  "Inversion not expected"
showGalcError ModeError =
  "Command not available in this mode"
showGalcError (ModuleRefError err) =
  "Invalide module name: " ++ err
showGalcError NoProofError = 
  "Currently there is no proof"
showGalcError (ExistingRefError err) =
  "Existing reference: " ++ err
showGalcError (QedError s1 s2) =
  "Cannot equalize: " ++ s1 ++ " and " ++ s2
showGalcError (NoOrderSetError) =
  "No order currently set"
showGalcError (RewriteError s) =
  "Cannot rewrite: " ++ s
showGalcError EqualityError =
  "Types are not equal"
showGalcError (NestedError e) =
  showGalcError e
showGalcError (DecomposeError s1 s2) = 
  "Cannot match types: " ++ s1 ++ " and " ++ s2
-------------------------------------------------------------------------------

