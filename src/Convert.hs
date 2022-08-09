{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Convert
  ( convert,
  )
where

import Control.Applicative
import Control.Carrier.Lift
import Control.Carrier.NonDet.Church
import Control.Carrier.Reader
import Control.Carrier.Throw.Either (runThrow)
import Control.Carrier.Trace.Printing
import Control.Effect.Throw
import Control.Effect.Trace
import Control.Monad
import Control.Monad.IO.Class (MonadIO)
import Data.Coerce
import Data.HashMap.Strict qualified as HashMap
import Data.List (findIndex, findIndices)
import Data.List.NonEmpty qualified as NE
import Data.Maybe
import Data.Monoid
import Data.String (fromString)
import Data.Text (Text)
import Data.Text.Optics
import Foreign (Ptr)
import GHC.Exts (IsList (..))
import JSON qualified
import Name qualified as Native
import Native qualified
import Optics
import TreeSitter.Language qualified as TS
import Witherable

type ConvertM sig m =
  ( MonadIO m,
    Has (Throw ParseError) sig m,
    Has Trace sig m,
    Has (Reader (Ptr TS.Language)) sig m,
    Has (Reader (NE.NonEmpty Native.Symbol)) sig m
  )

data ParseError
  = NoSubtypesPresent
  | UnexpectedAnonymousSubtype
  | SymbolIndexNotFound Text
  | UnexpectedAnonymousField Native.Name
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
nodeType i = choice <|> product <|> leaf <|> token <|> perish
  where
    token = Native.TokenNode <$> parseToken i
    leaf = Native.LeafNode <$> parseLeaf i
    choice = Native.ChoiceNode <$> parseChoice i
    product = Native.ProductNode <$> parseProduct i
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
  let leafName = n ^. #type % to Native.Name
  leafSymbolIndex <- findingIndexBy leafName
  pure Native.Leaf {..}

parseToken :: (ConvertM sig m, Alternative m) => JSON.NodeInfo -> m Native.Token
parseToken n = do
  guard (not (n ^. #named))
  let name = n ^. #type % to Native.Name
  indexed <- findingIndexBy name
  pure (Native.Token name (fromIntegral indexed))

parseProduct :: (ConvertM sig m, Alternative m) => JSON.NodeInfo -> m Native.Product
parseProduct n = do
  let fieldPairs = HashMap.toList (n ^. #fields & fromMaybe HashMap.empty)
  let children = n ^. #children
  when (null fieldPairs && isNothing children) empty

  productFields <- traverse (uncurry parseField) fieldPairs
  productExtras <- traverse (parseField "extraChildren") children
  let productName = n ^. #type % to Native.Name
  productIndices <- findingIndicesBy productName
  pure Native.Product {..}

parseField :: (ConvertM sig m) => Text -> JSON.FieldInfo -> m Native.Field
parseField n f = do
  let fieldName = Native.Name n
  fieldNature <- parseNature f
  fieldTypes <- traverse parseType (f ^. #types)
  pure Native.Field {..}

parseType :: (ConvertM sig m) => JSON.Node -> m Native.Name
parseType n = do
  let name = n ^. #type % to Native.Name
  unless (n ^. #named) (throwError (UnexpectedAnonymousField name))
  pure name

parseNature :: (ConvertM sig m) => JSON.FieldInfo -> m Native.Nature
parseNature i = pure $ case (i ^. #multiple, i ^. #required) of
  (True, True) -> Native.Some
  (True, False) -> Native.Many
  (False, True) -> Native.Optional
  (False, False) -> Native.Single

findingIndexBy :: ConvertM sig m => Native.Name -> m TS.TSSymbol
findingIndexBy name = do
  syms <- toList <$> ask @(NE.NonEmpty Native.Symbol)
  case findIndex (\s -> s ^. #name == name) syms of
    Just a -> pure (fromIntegral a)
    Nothing -> throwError (SymbolIndexNotFound (coerce name))

findingIndicesBy :: ConvertM sig m => Native.Name -> m [TS.TSSymbol]
findingIndicesBy name = do
  syms <- toList <$> ask @(NE.NonEmpty Native.Symbol)
  pure (fmap fromIntegral (findIndices (\s -> s ^. #name == name) syms))
