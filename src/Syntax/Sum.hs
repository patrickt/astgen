{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
module Syntax.Sum
  ( (:++:) (..)
  ) where

import Syntax.Kinds

type (:++:) :: Syntax -> Syntax -> Syntax
data (:++:) left right shape ann = LL1 (left shape ann) | RR1 (right shape ann)
  deriving stock Functor

infixr 5 :++:
