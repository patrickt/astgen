{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Syntax.Unmarshal
  ( SymbolMatching (..)
  , prettyNode
  ) where

import Syntax.Kinds
import Syntax.Sum
import Data.Proxy (Proxy (..))
import Prettyprinter (Doc, pretty)
import Prettyprinter qualified as Pretty
import TreeSitter.Node qualified as TS

class SymbolMatching (a :: Syntax) where
  matchedSymbols :: Proxy a -> [Int]
  showFailure :: forall x . Proxy a -> TS.Node -> Doc x

instance forall f g . (SymbolMatching f, SymbolMatching g) => SymbolMatching (f :++: g) where
  matchedSymbols _ = matchedSymbols (Proxy @f) <> matchedSymbols (Proxy @g)
  showFailure _ n = showFailure (Proxy @f) n <> Pretty.line <> showFailure (Proxy @g) n

prettyNode :: TS.Node -> Doc x
prettyNode n = Pretty.brackets start <> "-" <> Pretty.brackets end
  where
    start = pretty srow <> Pretty.comma <> pretty scol
    end = pretty erow <> Pretty.comma <> pretty ecol
    TS.TSPoint srow scol = TS.nodeStartPoint n
    TS.TSPoint erow ecol = TS.nodeEndPoint n
