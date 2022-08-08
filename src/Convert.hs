{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE MultiWayIf #-}
module Convert
  ( convert
  ) where

import JSON qualified
import Native qualified
import Data.String (fromString)
import Witherable
import GHC.Exts (IsList (..))
import Optics
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Control.Effect.Trace
import Control.Effect.Throw
import Control.Carrier.Lift
import Control.Carrier.NonDet.Church
import Control.Monad
import Data.Monoid
import Data.Coerce
import Control.Carrier.Trace.Printing
import Control.Applicative
import Control.Carrier.Throw.Either (runThrow)
import Data.Text.Optics

type ConvertM sig m = (Monad m, Has (Throw ParseError) sig m, Has Trace sig m)

data ParseError = NoSubtypesPresent | UnexpectedAnonymousSubtype
  deriving Show

convert :: String -> JSON.Document -> IO (Either ParseError Native.Document)
convert name
  = runM
  . runThrow
  . runTrace
  . document name


document :: ConvertM sig m => String -> JSON.Document -> m Native.Document
document langname (JSON.Document nts) = do
  let documentLanguageName = fromString langname
  documentDebugSymbols <- fromList . toList <$> traverse debugSymbol nts
  documentNodeTypes <- witherM (runNonDetA . nodeType) nts
  pure Native.Document{..}

nodeType :: (Alternative m, ConvertM sig m) => JSON.NodeInfo -> m Native.NodeType
nodeType i = choice <|> leaf <|> token <|> perish
  where
    token = Native.TokenNode <$> parseToken i
    leaf = Native.LeafNode <$> parseLeaf i
    choice = Native.ChoiceNode <$> parseChoice i
    perish = do
      -- trace ("Unrecognized node type " <> i ^. #type % unpacked)
      empty

parseChoice :: (ConvertM sig m, Alternative m) => JSON.NodeInfo -> m Native.Choice
parseChoice j =
  if
      | isJust (j ^. #subtypes) -> do
          case NE.nonEmpty (j ^.. #subtypes % folded % folded % #type) of
            Nothing -> throwError NoSubtypesPresent
            Just l -> do
              when (anyOf (#subtypes % folded % folded % #named) not j) (throwError UnexpectedAnonymousSubtype)
              let choiceName = j ^. #type % to Native.Name
              let choiceSubtypes = coerce l
              pure Native.Choice {..}
      | otherwise -> empty

parseLeaf :: (ConvertM sig m, Alternative m) => JSON.NodeInfo -> m Native.Leaf
parseLeaf n = do
  guard (n ^. #named)
  pure (Native.Leaf (n ^. #type % to Native.Name) 55)

parseToken :: (ConvertM sig m, Alternative m) => JSON.NodeInfo -> m Native.Token
parseToken n = do
  guard (not (n ^. #named))
  pure (Native.Token (n ^. #type % to Native.Name) 55)

debugSymbol :: ConvertM sig m => JSON.NodeInfo -> m Native.Symbol
debugSymbol n = do
  let symbolName = n ^. #type % to Native.Name
  let symbolNymity = if n ^. #named then Native.Named else Native.Anonymous
  pure Native.Symbol{..}
