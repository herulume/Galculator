
{-# LANGUAGE GADTs, Rank2Types #-}
{-# OPTIONS_GHC -Wall #-}

-------------------------------------------------------------------------------

{- |
Module      :  Language.R.Rewrite
Description :  Strategic rewriting combinators for the expression 
               representation.
Copyright   :  (c) Paulo Silva
License     :  LGPL

Maintainer  :  paufil@di.uminho.pt
Stability   :  experimental
Portability :  portable

-}

-------------------------------------------------------------------------------


module Language.R.Rewrite (
 GenericM,
 seqRules,
 nop,
 failM,
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
 all,
 one
 ) where 

import Control.MonadOr
import Control.Monad.State
import Language.R.Spine
import Language.R.Syntax
import Language.Type.Syntax
import Prelude hiding (all)

-------------------------------------------------------------------------------

type GenericM m = forall a . Type a -> R a -> m (R a)

-------------------------------------------------------------------------------

seqRules :: MonadOr m => [GenericM m] -> GenericM m
seqRules [] = failM
seqRules (x:xs) = x |<| seqRules xs 

-------------------------------------------------------------------------------

nop :: Monad m => GenericM m
nop _ = return

-------------------------------------------------------------------------------

failM :: MonadPlus m => GenericM m
failM _ _ = mzero

-------------------------------------------------------------------------------

(>>>) :: Monad m => GenericM m -> GenericM m -> GenericM m
(f >>> g) t = f t >=> g t

-------------------------------------------------------------------------------

(|||) :: MonadPlus m => GenericM m -> GenericM m -> GenericM m
(f ||| g) t x = f t x `mplus` g t x

-------------------------------------------------------------------------------

(|<|) :: MonadOr m => GenericM m -> GenericM m -> GenericM m
(f |<| g) t x = f t x `morelse` g t x

-------------------------------------------------------------------------------

many :: MonadOr m => GenericM m -> GenericM m
many r = (r >>> many r) |<| nop

-------------------------------------------------------------------------------

many1 :: MonadOr m => GenericM m -> GenericM m
many1 r = r >>> many r

-------------------------------------------------------------------------------

try :: MonadOr m => GenericM m -> GenericM m
try x = x |<| nop

-------------------------------------------------------------------------------

once :: MonadOr m => GenericM m -> GenericM m
once f = f |<| one (once f)

-------------------------------------------------------------------------------

everywhere :: Monad m => GenericM m -> GenericM m
everywhere f = f >>> all (everywhere f)

-------------------------------------------------------------------------------

everywhere' :: Monad m => GenericM m -> GenericM m
everywhere' f = all (everywhere' f) >>> f

-------------------------------------------------------------------------------

innermost :: MonadOr m => GenericM m -> GenericM m
innermost f = all (innermost f) >>> try (f >>> innermost f)

-------------------------------------------------------------------------------

all :: Monad m => GenericM m -> GenericM m
all strg typ expr = do
  s <- aux strg (toSpine typ expr)
  return (fromSpine s)
  where
    aux :: Monad m => GenericM m -> (Spine a -> m (Spine a))
    aux _ x@(Constr _) = return x
    aux g (f `Ap` (t :| x)) = do
      h <- aux g f
      y <- g t x
      return (h `Ap` (t :| y))

-------------------------------------------------------------------------------

one :: MonadOr m => GenericM m -> GenericM m
one strg typ expr = do
  s <- aux strg (toSpine typ expr)
  return (fromSpine s)
  where
    aux :: MonadOr m => GenericM m -> (Spine a -> m (Spine a))
    aux _ (Constr _) = mzero
    aux g (f `Ap` (t :| x)) = 
      (do h <- aux g f
          return $ h `Ap` (t :| x))
      `morelse`
      (do j <- g t x
          return $ f `Ap` (t :| j))

-------------------------------------------------------------------------------
