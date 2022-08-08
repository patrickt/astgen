{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Syntax.SFunctor
  ( SFunctor (..)
  ) where

import Syntax.Sum
import Syntax.Kinds

class SFunctor (s :: Syntax) where
  smap :: (Functor f, Functor g) => (forall a . f a -> g a) -> s f ann -> s g ann

instance (SFunctor f, SFunctor g) => SFunctor (f :++: g) where
  smap f (LL1 x) = LL1 (smap f x)
  smap f (RR1 x) = RR1 (smap f x)
