
{-# LANGUAGE GADTs, Rank2Types #-}
{-# OPTIONS_GHC -Wall #-}
 
-------------------------------------------------------------------------------

{- |
Module      :  Language.Type.Rewrite
Description :  Strategic rewriting combinators for the type representation.
Copyright   :  (c) Paulo Silva
License     :  LGPL
  
Maintainer  :  paufil@di.uminho.pt
Stability   :  experimental
Portability :  portable
 
-}

-------------------------------------------------------------------------------

module Language.Type.Rewrite (
  Rule,
  View(View),
  showType,
  view2Box,
  nop,
  fail,
  seqRules,
  (>>>),
  (|||),
  (|<|),
  many,
  many1,
  try,
  once,
  everywhere,
  everywhere',
  innermost,
  one,
  all
 ) where

import Control.MonadOr
import Control.Monad hiding (fail)
import Data.Existential
import Language.Type.Syntax
import Prelude hiding (all,fail)

-------------------------------------------------------------------------------

data View a where
  View :: Type b -> View (Type a)

instance Show (View a) where
  show (View b) = "View: " ++ show b

-------------------------------------------------------------------------------

showType :: View a -> String
showType (View b) = show b

-------------------------------------------------------------------------------

view2Box :: View (Type b) -> TypeBox
view2Box (View t) = Hide t

-------------------------------------------------------------------------------

type Rule = forall a . forall m . MonadOr m => Type a -> m (View (Type a))

-------------------------------------------------------------------------------

nop :: Rule
nop = return . View

-------------------------------------------------------------------------------

fail :: Rule
fail _ = mzero

-------------------------------------------------------------------------------

seqRules :: [Rule] -> Rule
seqRules [] = fail
seqRules (x:xs) = x |<| seqRules xs

-------------------------------------------------------------------------------

(>>>) :: Rule -> Rule -> Rule
(f >>> g) a = do
  View b <- f a
  View c <- g b
  return $ View c

-------------------------------------------------------------------------------

(|||) :: Rule -> Rule -> Rule
(f ||| g) x = f x `mplus` g x

-------------------------------------------------------------------------------

(|<|) :: Rule -> Rule -> Rule
(f |<| g) x = f x `morelse` g x

-------------------------------------------------------------------------------

many :: Rule -> Rule
many r = (r >>> many r) |<| nop

-------------------------------------------------------------------------------

many1 :: Rule -> Rule
many1 r = r >>> many r

-------------------------------------------------------------------------------

try :: Rule -> Rule
try r = r |<| nop

-------------------------------------------------------------------------------

once :: Rule -> Rule
once f = f |<| one (once f)

-------------------------------------------------------------------------------

everywhere :: Rule -> Rule
everywhere r = r >>> all (everywhere r)

-------------------------------------------------------------------------------

everywhere' :: Rule -> Rule
everywhere' r = all (everywhere' r) >>> r

-------------------------------------------------------------------------------

innermost :: Rule -> Rule
innermost r = all (innermost r) >>> try (r >>> innermost r)

-------------------------------------------------------------------------------

all :: Rule -> Rule
all _ x@(TVar _) = return $ View x
all _ One = return $ View One
all _ Bool = return $ View Bool
all _ Char = return $ View Char
all _ String = return $ View String
all _ Int  = return $ View Int
all _ Float = return $ View Float 
all r (Prod a b) = do
  View a' <- r a
  View b' <- r b
  return $ View (Prod a' b')
all r (Either a b) = do
  View a' <- r a
  View b' <- r b
  return $ View (Either a' b')
all r (Maybe a) = do
  View a' <- r a
  return $ View (Maybe a')
all r (List a) = do
  View a' <- r a
  return $ View (List a')
all r (Set a) = do
  View a' <- r a
  return $ View (Set a')
all r (Map a b) = do
  View a' <- r a
  View b' <- r b
  return $ View (Map a' b')
all r (Fun a b) = do
  View a' <- r a
  View b' <- r b 
  return $ View (Fun a' b')
all r (Rel a b) = do
  View a' <- r a
  View b' <- r b
  return $ View (Rel a' b')
all r (Ord a) = do
  View a' <- r a
  return $ View (Ord a')
all r (GC a b) = do
  View a' <- r a
  View b' <- r b
  return $ View (GC a' b')

-------------------------------------------------------------------------------

one :: Rule -> Rule
one _ (TVar _) = mzero
one _ One = mzero
one _ Bool = mzero
one _ Char = mzero
one _ String = mzero
one _ Int = mzero
one _ Float = mzero
one r (Prod a b) =
  (do View c <- r a
      return $ View (Prod c b)) `morelse`
  (do View c <- r b
      return $ View (Prod a c))
one r (Either a b) = 
  (do View c <- r a
      return $ View (Either c b)) `morelse`
  (do View c <- r b
      return $ View (Prod a c))
one r (Maybe a) = 
  (do View c <- r a
      return $ View (Maybe c))
one r (List a) = 
  (do View c <- r a
      return $ View (List c))
one r (Set a) = 
  (do View c <- r a
      return $ View (Set c))
one r (Map a b) =
  (do View c <- r a
      return $ View (Map c b)) `morelse`
  (do View c <- r b
      return $ View (Map a c))
one r (Fun a b) =
  (do View c <- r a
      return $ View (Fun c b)) `morelse`
  (do View c <- r b
      return $ View (Fun a c))
one r (Rel a b) =
  (do View c <- r a
      return $ View (Rel c b)) `morelse`
  (do View c <- r b
      return $ View (Rel a c))
one r (Ord a) = 
  (do View c <- r a
      return $ View (Ord c))
one r (GC a b) = 
  (do View c <- r a
      return $ View (GC c b)) `morelse`
  (do View c <- r b
      return $ View (GC a c))

-------------------------------------------------------------------------------

