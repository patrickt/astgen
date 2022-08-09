{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Language.JSON.AST (module Language.JSON.AST) where

import Data.Coerce (coerce)
import qualified Data.List
import Data.Semigroup ((<>))
import qualified Data.Text
import Prettyprinter ((<+>))
import qualified Prettyprinter
import qualified Syntax
import qualified Syntax.Kinds
import Syntax.Sum ((:++:))
import qualified TreeSitter.Node
import Prelude ((.))
import qualified Prelude

debugSymbolNames :: [Prettyprinter.Doc a]
debugSymbolNames = ["end", "_{", "_,", "_}", "_:", "_[", "_]", "_\"", "_string_content_token1", "escape_sequence", "number", "true", "false", "null", "document", "_value", "object", "pair", "array", "string", "string_content", "_object_repeat1", "_array_repeat1", "_string_content_repeat1"]

type Value :: Syntax.Kinds.Choice
newtype Value f a = Value {unValue :: (Array :++: False :++: Null :++: Number :++: Object :++: String :++: True) f a}
  deriving stock (Prelude.Functor)
  deriving newtype (Syntax.SFunctor, Syntax.SymbolMatching)

instance Syntax.STraversable Value where straverse f (Value x) = Prelude.fmap Value (Syntax.straverse f x)

type Array :: Syntax.Kinds.Leaf
data Array f a = Array {ann :: a, text :: Data.Text.Text}
  deriving stock (Prelude.Functor)

instance Syntax.SFunctor Array where smap _ = coerce

instance Syntax.STraversable Array where straverse _ = Prelude.pure . coerce

instance Syntax.SymbolMatching Array where
  matchedSymbols _ = [18]
  showFailure _ node = "expected array but got" <+> found <+> Prettyprinter.parens (Syntax.prettyNode node)
    where
      found = Data.List.genericIndex debugSymbolNames (TreeSitter.Node.nodeSymbol node)

type Document :: Syntax.Kinds.Leaf
data Document f a = Document {ann :: a, text :: Data.Text.Text}
  deriving stock (Prelude.Functor)

instance Syntax.SFunctor Document where smap _ = coerce

instance Syntax.STraversable Document where straverse _ = Prelude.pure . coerce

instance Syntax.SymbolMatching Document where
  matchedSymbols _ = [14]
  showFailure _ node = "expected document but got" <+> found <+> Prettyprinter.parens (Syntax.prettyNode node)
    where
      found = Data.List.genericIndex debugSymbolNames (TreeSitter.Node.nodeSymbol node)

type Object :: Syntax.Kinds.Leaf
data Object f a = Object {ann :: a, text :: Data.Text.Text}
  deriving stock (Prelude.Functor)

instance Syntax.SFunctor Object where smap _ = coerce

instance Syntax.STraversable Object where straverse _ = Prelude.pure . coerce

instance Syntax.SymbolMatching Object where
  matchedSymbols _ = [16]
  showFailure _ node = "expected object but got" <+> found <+> Prettyprinter.parens (Syntax.prettyNode node)
    where
      found = Data.List.genericIndex debugSymbolNames (TreeSitter.Node.nodeSymbol node)

type Pair :: Syntax.Kinds.Leaf
data Pair f a = Pair {ann :: a, text :: Data.Text.Text}
  deriving stock (Prelude.Functor)

instance Syntax.SFunctor Pair where smap _ = coerce

instance Syntax.STraversable Pair where straverse _ = Prelude.pure . coerce

instance Syntax.SymbolMatching Pair where
  matchedSymbols _ = [17]
  showFailure _ node = "expected pair but got" <+> found <+> Prettyprinter.parens (Syntax.prettyNode node)
    where
      found = Data.List.genericIndex debugSymbolNames (TreeSitter.Node.nodeSymbol node)

type String :: Syntax.Kinds.Leaf
data String f a = String {ann :: a, text :: Data.Text.Text}
  deriving stock (Prelude.Functor)

instance Syntax.SFunctor String where smap _ = coerce

instance Syntax.STraversable String where straverse _ = Prelude.pure . coerce

instance Syntax.SymbolMatching String where
  matchedSymbols _ = [19]
  showFailure _ node = "expected string but got" <+> found <+> Prettyprinter.parens (Syntax.prettyNode node)
    where
      found = Data.List.genericIndex debugSymbolNames (TreeSitter.Node.nodeSymbol node)

type StringContent :: Syntax.Kinds.Leaf
data StringContent f a = StringContent {ann :: a, text :: Data.Text.Text}
  deriving stock (Prelude.Functor)

instance Syntax.SFunctor StringContent where smap _ = coerce

instance Syntax.STraversable StringContent where straverse _ = Prelude.pure . coerce

instance Syntax.SymbolMatching StringContent where
  matchedSymbols _ = [20]
  showFailure _ node = "expected string_content but got" <+> found <+> Prettyprinter.parens (Syntax.prettyNode node)
    where
      found = Data.List.genericIndex debugSymbolNames (TreeSitter.Node.nodeSymbol node)

type AnonymousDQuote :: Syntax.Kinds.Token
type AnonymousDQuote = Syntax.Token "\"" 7

type AnonymousComma :: Syntax.Kinds.Token
type AnonymousComma = Syntax.Token "," 2

type AnonymousColon :: Syntax.Kinds.Token
type AnonymousColon = Syntax.Token ":" 4

type AnonymousLBracket :: Syntax.Kinds.Token
type AnonymousLBracket = Syntax.Token "[" 5

type AnonymousRBracket :: Syntax.Kinds.Token
type AnonymousRBracket = Syntax.Token "]" 6

type EscapeSequence :: Syntax.Kinds.Leaf
data EscapeSequence f a = EscapeSequence {ann :: a, text :: Data.Text.Text}
  deriving stock (Prelude.Functor)

instance Syntax.SFunctor EscapeSequence where smap _ = coerce

instance Syntax.STraversable EscapeSequence where straverse _ = Prelude.pure . coerce

instance Syntax.SymbolMatching EscapeSequence where
  matchedSymbols _ = [9]
  showFailure _ node = "expected escape_sequence but got" <+> found <+> Prettyprinter.parens (Syntax.prettyNode node)
    where
      found = Data.List.genericIndex debugSymbolNames (TreeSitter.Node.nodeSymbol node)

type False :: Syntax.Kinds.Leaf
data False f a = False {ann :: a, text :: Data.Text.Text}
  deriving stock (Prelude.Functor)

instance Syntax.SFunctor False where smap _ = coerce

instance Syntax.STraversable False where straverse _ = Prelude.pure . coerce

instance Syntax.SymbolMatching False where
  matchedSymbols _ = [12]
  showFailure _ node = "expected false but got" <+> found <+> Prettyprinter.parens (Syntax.prettyNode node)
    where
      found = Data.List.genericIndex debugSymbolNames (TreeSitter.Node.nodeSymbol node)

type Null :: Syntax.Kinds.Leaf
data Null f a = Null {ann :: a, text :: Data.Text.Text}
  deriving stock (Prelude.Functor)

instance Syntax.SFunctor Null where smap _ = coerce

instance Syntax.STraversable Null where straverse _ = Prelude.pure . coerce

instance Syntax.SymbolMatching Null where
  matchedSymbols _ = [13]
  showFailure _ node = "expected null but got" <+> found <+> Prettyprinter.parens (Syntax.prettyNode node)
    where
      found = Data.List.genericIndex debugSymbolNames (TreeSitter.Node.nodeSymbol node)

type Number :: Syntax.Kinds.Leaf
data Number f a = Number {ann :: a, text :: Data.Text.Text}
  deriving stock (Prelude.Functor)

instance Syntax.SFunctor Number where smap _ = coerce

instance Syntax.STraversable Number where straverse _ = Prelude.pure . coerce

instance Syntax.SymbolMatching Number where
  matchedSymbols _ = [10]
  showFailure _ node = "expected number but got" <+> found <+> Prettyprinter.parens (Syntax.prettyNode node)
    where
      found = Data.List.genericIndex debugSymbolNames (TreeSitter.Node.nodeSymbol node)

type True :: Syntax.Kinds.Leaf
data True f a = True {ann :: a, text :: Data.Text.Text}
  deriving stock (Prelude.Functor)

instance Syntax.SFunctor True where smap _ = coerce

instance Syntax.STraversable True where straverse _ = Prelude.pure . coerce

instance Syntax.SymbolMatching True where
  matchedSymbols _ = [11]
  showFailure _ node = "expected true but got" <+> found <+> Prettyprinter.parens (Syntax.prettyNode node)
    where
      found = Data.List.genericIndex debugSymbolNames (TreeSitter.Node.nodeSymbol node)

type AnonymousLBrace :: Syntax.Kinds.Token
type AnonymousLBrace = Syntax.Token "{" 1

type AnonymousRBrace :: Syntax.Kinds.Token
type AnonymousRBrace = Syntax.Token "}" 3
