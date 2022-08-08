{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Syntax.STraversable
  ( STraversable (..)
  ) where

import Syntax.Kinds
import Syntax.Sum

class STraversable (s :: Syntax) where
  straverse :: forall f g m . (Traversable f, Traversable g, Monad m) =>
    (forall x . f x -> m (g x)) ->
    forall x . s f x ->
    m (s g x)

instance (STraversable f, STraversable g) => STraversable (f :++: g) where
  straverse f (LL1 x) = LL1 <$> straverse f x
  straverse f (RR1 x) = RR1 <$> straverse f x
