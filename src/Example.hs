{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE CPP #-}

module Example where

import Prelude hiding (String, Bool (..))
import Data.Text (Text)
import Data.Kind
import Data.Coerce
import Control.Monad
import Syntax

-- Leaf nodes


#define LEAF_NODE(NAME) type NAME :: Leaf; \
  data NAME f a = NAME { ann :: a, text :: Text } deriving (Functor); \
  instance SFunctor NAME where { smap _ = coerce; }; \
  instance STraversable NAME where { straverse _ = pure . coerce; }

LEAF_NODE(Number)
LEAF_NODE(True)
LEAF_NODE(False)
LEAF_NODE(Null)
LEAF_NODE(EscapeSequence)

--


#define OPTIONAL_MULTIPLE_NODE(NAME, CHILD) type NAME :: Node; \
  data NAME f a = NAME { ann :: a, extraChildren :: [f (NAME f a)] } deriving (Functor); \
  instance SFunctor NAME where { smap f (NAME ann v) = NAME ann (fmap (f . fmap (smap f)) v); }; \
  instance STraversable NAME where { straverse f (NAME ann vals) = NAME ann <$> traverse (f >=> traverse (straverse f)) vals; };

OPTIONAL_MULTIPLE_NODE(Array, Value)
OPTIONAL_MULTIPLE_NODE(StringContent, EscapeSequence)
OPTIONAL_MULTIPLE_NODE(Object, Pair)

#define OPTIONAL_SINGLE_NODE(NAME, CHILD) type NAME :: Node; \
  data NAME f a = NAME { ann :: a, extraChildren :: Maybe (f (NAME f a)) } deriving (Functor); \
  instance SFunctor NAME where { smap f = go where go (NAME ann v) = NAME ann (fmap (f . fmap go) v); }; \
  instance STraversable NAME where { straverse f = go where go (NAME ann vals) = NAME ann <$> traverse (f >=> traverse go) vals; };

OPTIONAL_SINGLE_NODE(String, StringContent)

type Pair :: Node
data Pair f a = Pair a (f ((Number :++: String) f a)) (f (Value f a))
  deriving stock Functor

instance SFunctor Pair where
  smap f (Pair a l r) = Pair a (f (fmap (smap f) l)) (f (fmap (smap f) r))

instance STraversable Pair where
  straverse :: forall f g m . (Traversable f, Traversable g, Monad m) => (forall x . f x -> m (g x)) -> forall x . Pair f x -> m (Pair g x)
  straverse f (Pair ann l r) = Pair ann <$> (f l >>= traverse (straverse f)) <*> (f r >>= traverse (straverse f))



#define CHOICE_NODE(NAME, WRAPS) type NAME :: Choice; \
  newtype NAME f a = NAME { unNAME :: (WRAPS) f a } deriving Functor; \
  deriving newtype instance SFunctor NAME; \
  instance STraversable NAME where { straverse f (NAME x) = NAME <$> straverse f x; };

CHOICE_NODE(Value, Array :++: False :++: Null :++: Number :++: Object :++: String)
