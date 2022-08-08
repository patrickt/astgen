{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
module Syntax.Unmarshal
  ( SymbolMatching (..)
  ) where

import Syntax.Kinds
import Syntax.Sum
import Data.Proxy (Proxy (..))
import Prettyprinter (Doc)
import Prettyprinter qualified as Pretty

class SymbolMatching (a :: Syntax) where
  matchedSymbols :: Proxy a -> [Int]
  showFailure :: forall x . Proxy a -> Node -> Doc x

instance forall f g . (SymbolMatching f, SymbolMatching g) => SymbolMatching (f :++: g) where
  matchedSymbols _ = matchedSymbols (Proxy @f) <> matchedSymbols (Proxy @g)
  showFailure _ n = showFailure (Proxy @f) n <> Pretty.line <> showFailure (Proxy @g) n
