{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Render where

import Data.Foldable
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Builder.Linear (Builder)
import Data.Text.Builder.Linear qualified as Builder
import Data.List.NonEmpty (NonEmpty (..))
import Native
import Name (Name)
import Name qualified
import Optics
import Optics.Label ()
import TreeSitter.Symbol (toHaskellPascalCaseIdentifier)
import Data.Coerce
import Data.Maybe
import Debug.Trace (traceShow, traceShowId)

class Render a where
  render :: a -> Builder

instance Render Document where
  render (Document name (debug :| rest) allNodes) =
    let allDecls = foldl' (\x y -> x <> "\n\n" <> y) mempty (fmap render allNodes)
        debugList = "[" <> foldl' (\x y -> x <> "," <> render y) (render debug) rest <> "]"
     in [i|
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators #-}
module Language.#{name}.AST (module Language.#{name}.AST) where

import Prelude ((.))
import qualified Prelude

import Data.Coerce (coerce)
import Data.Semigroup ((<>))
import Prettyprinter ((<+>))
import Syntax.Sum ((:++:))
import qualified Data.List
import qualified Data.Text
import qualified Prettyprinter
import qualified Syntax
import qualified Syntax.Kinds
import qualified TreeSitter.Node

debugSymbolNames :: [Prettyprinter.Doc a]
debugSymbolNames = |]
      <> debugList
      <> allDecls

foldedIntoSum :: [Name] -> Text
foldedIntoSum ns = surround (foldl1 glue allNames)
  where
    allNames = fmap Name.toPascalCase ns
    glue :: Text -> Text -> Text
    glue l r = l <> " :++: " <> r
    surround s
      | length ns == 1 = s
      | otherwise = "(" <> s <> ")"


instance Render Choice where
  render c =
    let name :: Text
        name = c ^. #name % to Name.asConstructor
        choices :: Text
        choices = foldedIntoSum (c ^.. #subtypes % folded % coerced)
     in [i|
type #{name} :: Syntax.Kinds.Choice
newtype #{name} f a = #{name} { un#{name} :: #{choices} f a }
  deriving stock Prelude.Functor
  deriving newtype (Syntax.SFunctor, Syntax.SymbolMatching)

instance Syntax.STraversable #{name} where straverse f (#{name} x) = Prelude.fmap #{name} (Syntax.straverse f x)
|]

instance Render Leaf where
  render (Leaf lname sym) =
    let
      name :: String
      name = toHaskellPascalCaseIdentifier (Text.unpack (coerce lname))
    in [i|
type #{name} :: Syntax.Kinds.Leaf
data #{name} f a = #{name} { ann :: a, text :: Data.Text.Text }
  deriving stock Prelude.Functor

instance Syntax.SFunctor #{name} where smap _ = coerce
instance Syntax.STraversable #{name} where straverse _ = Prelude.pure . coerce
instance Syntax.SymbolMatching #{name} where
  matchedSymbols _ = [#{sym}]
  showFailure _ node = "expected #{lname} but got" <+> found <+> Prettyprinter.parens (Syntax.prettyNode node)
    where
      found = Data.List.genericIndex debugSymbolNames (TreeSitter.Node.nodeSymbol node)
|]

instance Render Nature where
  render = \case
    Single -> ""
    Optional -> "Maybe"
    Many -> "[]"
    Some -> "NonEmpty"

instance Render Token where
  render (Token name symbol) =
    let readable = Name.toPascalCase name
        display = Name.escaped name
     in [i|
type Anonymous#{readable} :: Syntax.Kinds.Token
type Anonymous#{readable} = Syntax.Token "#{display}" #{symbol}
|]

instance Render Product where
  render p =
    let
      name = p ^. #name % to Name.toPascalCase
      extraChildren = case p ^. #extras of
        Nothing -> ""
        Just a -> "\n, " <> render a
      allFields = case p ^. #fields of
        [] -> ""
        [a] -> "\n, " <> render a
        as -> foldl' (\b f -> b <> "\n, " <> render f) "" as
    in [i|
type #{name} :: Syntax.Kinds.Product
data #{name} f a = #{name}
  { ann :: a |] <> extraChildren <> allFields <> "\n} deriving stock (Functor)"

instance Render Field where
  render f =
    let
      name = f ^. #name % to Name.camelCase % to render
      sum = foldedIntoSum (f ^.. #types % folded)
      wrapperFor x = case f ^. #nature of
        Single -> "f (" <> x <> " f a)"
        Optional -> "Prelude.Maybe (f (" <> x <> " f a))"
        Some -> "NonEmpty.NonEmpty (f (" <> x <> " f a))"
        Many -> "[f (" <> x <> " f a)]"
    in name <> " :: " <> wrapperFor (render sum)

instance Render NodeType where
  render = \case
    ChoiceNode c -> render c
    LeafNode l -> render l
    TokenNode t -> render t
    ProductNode p -> render p

instance Render Symbol where
  render s = "\"" <> full <> "\""
    where
      full = leading <> name
      leading = if isAnonymous s then "_" else ""
      name = s ^. #name % to Name.escaped % to Builder.fromText

instance Render Name.Name where
  render = coerce Builder.fromText

instance Render Text where
  render = Builder.fromText
