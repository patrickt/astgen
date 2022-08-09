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
module Language.#{name}.AST (module Language.#{name}.AST) where

import Prelude ()
import qualified Prelude
import qualified Data.List
import Syntax.Unmarshal
import Prettyprinter ((<+>))
import Data.Semigroup ((<>))
import qualified Prettyprinter

debugSymbolNames :: Prettyprinter.Doc a
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
newtype #{name} f a = #{name} { un#{name} :: (#{choices}) f a }
  deriving stock Prelude.Functor
  deriving newtype (SFunctor, SymbolMatching)

instance STraversable #{name} where straverse f (#{name} x) = #{name} <$> straverse f x
|]

instance Render Leaf where
  render (Leaf lname sym) =
    let
      name :: String
      name = toHaskellPascalCaseIdentifier (Text.unpack (coerce lname))
    in [i|
data #{name} f a = #{name} { ann :: a, text :: Data.Text.Text }
  deriving stock Prelude.Functor

instance SFunctor #{name} where smap _ = coerce
instance STraversable #{name} where straverse _ = pure . coerce
instance SymbolMatching #{name} where
  matchedSymbols _ = [#{sym}]
  showFailure _ node = "expected #{lname} but got" <+> found <+> Pretty.parens (prettyNode node)
    where
      found = Data.List.genericIndex debugSymbolNames (TS.nodeSymbol node)
|]

natureWrapper :: Nature -> Builder
natureWrapper = \case
  Single -> ""
  Optional -> "Maybe"
  Many -> "[]"
  Some -> "NonEmpty"

instance Render Token where
  render (Token (Name.Name name) symbol) =
    let readable = toHaskellPascalCaseIdentifier (Text.unpack name)
        escaped = if name == "\"" then "\\\"" else name
     in [i|
type Anonymous#{readable} = Syntax.Token.Token "#{escaped}" #{symbol}
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
      name = s ^. #name % coerced % to Builder.fromText

instance Render Name.Name where
  render = coerce Builder.fromText
