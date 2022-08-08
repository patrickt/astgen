{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
module Render where

import Native
import Data.Text.Builder.Linear (Builder)
import Data.Text.Builder.Linear qualified as Builder
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Foldable
import Optics
import Optics.Label ()

class Render a where
  render :: a -> Builder

instance Render Document where
  render (Document ds) = foldl' (\x y -> x <> "\n\n" <> y) mempty (fmap render ds)

instance Render Choice where
  render c =
    let
      name :: Name
      name = c ^. #name
      choices :: Text
      choices = foldl1 (\x y -> x <> " :++: " <> y) (c ^.. #subtypes % folded % coerced)
    in [i|
newtype #{name} f a = #{name} { un#{name} :: #{choices} }
  deriving stock Functor
  deriving newtype SFunctor

instance STraversable #{name} where straverse f (#{name} x) = #{name} <$> straverse f x
|]

instance Render Leaf where
  render (Leaf name) = [i|
data #{name} f a = #{name} { ann :: a, text :: Data.Text.Text }
  deriving stock Functor

instance SFunctor #{name} where smap _ = coerce
instance STraversable #{name} where straverse _ = pure . coerce
|]

natureWrapper :: Nature -> Builder
natureWrapper = \case
  Single -> ""
  Optional -> "Maybe"
  Many -> "[]"
  Some -> "NonEmpty"

instance Render Token where
  render (Token (Name name) symbol) =
    let
      readable = toHaskellPascalCaseIdentifier (Text.unpack name)
    in [i|
type Anonymous#{readable} = Syntax.Token.Token "#{name}" #{symbol}
|]

instance Render Product where render = error "TODO"

instance Render NodeType where
  render = \case
    ChoiceNode c -> render c
    LeafNode l -> render l
    TokenNode t -> render t
    ProductNode p -> render p
