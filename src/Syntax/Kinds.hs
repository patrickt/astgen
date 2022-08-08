-- | Defines human-readable kinds for different syntax types.
-- These are actually all the same kind, but are there for
-- readability purposes.
module Syntax.Kinds
  ( Syntax
  , Leaf
  , Node
  ) where

import Data.Kind (Type)

type Syntax = (Type -> Type) -> (Type -> Type)

type Leaf = (Type -> Type) -> (Type -> Type)

type Node = (Type -> Type) -> (Type -> Type)