{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Render where

import Data.Foldable
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Builder.Linear (Builder)
import Data.Text.Builder.Linear qualified as Builder
import Data.List.NonEmpty (NonEmpty (..))
import Native
import Name qualified
import Optics
import Optics.Label ()
import TreeSitter.Symbol (toHaskellPascalCaseIdentifier)
import Data.Coerce

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

instance Render Choice where
  render c =
    let name :: Text
        name = c ^. #name % to Name.asConstructor
        glue :: Text -> Text -> Text
        glue l r = Text.toTitle l <> " :++: " <> Text.toTitle r
        choices :: Text
        choices = foldl1 glue (c ^.. #subtypes % folded % coerced)
     in [i|
type #{name} :: Syntax.Kinds.Choice
newtype #{name} f a = #{name} { un#{name} :: (#{choices}) f a }
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

natureWrapper :: Nature -> Builder
natureWrapper = \case
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

instance Render Product where render = error "TODO"

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
