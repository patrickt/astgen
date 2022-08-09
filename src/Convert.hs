{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Convert
  ( convert,
  )
where

import Control.Applicative
import Control.Carrier.Lift
import Control.Carrier.NonDet.Church
import Control.Carrier.Throw.Either (runThrow)
import Control.Carrier.Trace.Printing
import Control.Effect.Throw
import Control.Effect.Trace
import Control.Monad
import Data.Coerce
import Data.List.NonEmpty qualified as NE
import Data.Maybe
import Data.Monoid
import Data.String (fromString)
import Data.Text.Optics
import GHC.Exts (IsList (..))
import JSON qualified
import Native qualified
import Name qualified as Native
import Optics
import Witherable
import qualified TreeSitter.Language as TS
import Foreign (Ptr)
import Control.Carrier.Reader
import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import Data.List (findIndex)

type ConvertM sig m =
  ( MonadIO m,
    Has (Throw ParseError) sig m,
    Has Trace sig m,
    Has (Reader (Ptr TS.Language)) sig m,
    Has (Reader (NE.NonEmpty Native.Symbol)) sig m
  )

data ParseError = NoSubtypesPresent | UnexpectedAnonymousSubtype | SymbolIndexNotFound Text
  deriving (Show)

convert :: Ptr TS.Language -> String -> JSON.Document -> IO (Either ParseError Native.Document)
convert lang name doc = do
  syms <- Native.allSymbols lang
  runM
    . runThrow
    . runTrace
    . runReader lang
    . runReader syms
    $ document name doc

document :: ConvertM sig m => String -> JSON.Document -> m Native.Document
document langname (JSON.Document nts) = do
  let documentLanguageName = fromString langname
  -- documentDebugSymbols <- fromList . toList <$> traverse debugSymbol nts
  documentDebugSymbols <- ask >>= Native.allSymbols
  documentNodeTypes <- witherM (runNonDetA . nodeType) nts
  pure Native.Document {..}

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
  syms <- toList <$> ask @(NE.NonEmpty Native.Symbol)
  let name = n ^. #type % to Native.Name
  indexed <-
    case findIndex (\s -> s ^. #name == name) syms of
      Just a -> pure a
      Nothing -> throwError (SymbolIndexNotFound (coerce name))
  pure (Native.Token name (fromIntegral indexed))
