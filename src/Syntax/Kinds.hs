-- | Defines human-readable kinds for different syntax types.
-- These are actually all the same kind, but are there for
-- readability purposes.
module Syntax.Kinds
  ( Syntax
  , Leaf
  , Node
  , Choice
  , Token
  ) where

import Data.Kind (Type)

type Syntax = (Type -> Type) -> (Type -> Type)

type Leaf = Syntax

type Node = Syntax

type Choice = Syntax

-- Tokens are not parameterized by a shape functor.
type Token = Type -> Type
