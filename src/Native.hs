{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module Native (module Native) where

import Control.Effect.Error
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bool (bool)
import Data.Coerce
import Data.Generics.Product.Typed
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Maybe
import Data.String (IsString, fromString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Typeable (Typeable)
import Foreign (Ptr)
import Foreign.C.String (peekCString)
import JSON qualified
import Optics
import Optics.Label ()
import Optics.TH
import TreeSitter.Language qualified as TS
import TreeSitter.Symbol (TSSymbol)
import GHC.Generics (Generic)
import Data.Vector (Vector)

data Document = Document
  { documentLanguageName :: Name,
    documentDebugSymbols :: NonEmpty Symbol,
    documentNodeTypes :: Vector NodeType
  }

newtype Name = Name Text
  deriving newtype (IsString)

instance Show Name where
  show = Text.unpack . coerce

data Nature = Single | Optional | Some | Many

data Nymity = Named | Anonymous
  deriving stock (Eq)

isAnonymous :: HasType Nymity s => s -> Bool
isAnonymous s = s ^. typed @Nymity == Anonymous

data NodeType
  = ChoiceNode Choice
  | LeafNode Leaf
  | TokenNode Token
  | ProductNode Product

data Choice = Choice
  { choiceName :: Name,
    choiceSubtypes :: NonEmpty Name
  }

data Leaf = Leaf Name TSSymbol

data Token = Token Name TSSymbol

data Product = Product Name Nature [Name] [TSSymbol]

data Symbol = Symbol {symbolName :: Name, symbolNymity :: Nymity}
  deriving stock (Generic)

makeFieldLabels ''Choice
makeFieldLabels ''Symbol

data TSException = ZeroLanguageSymbolsPresent
  deriving stock (Typeable, Show)

instance Exception TSException where
  displayException = \case
    ZeroLanguageSymbolsPresent -> "Zero language symbols fetched for language"


allSymbols :: MonadIO m => Ptr TS.Language -> m (NonEmpty Symbol)
allSymbols language = liftIO do
  count <- TS.ts_language_symbol_count language
  when (count == 0) (throwIO ZeroLanguageSymbolsPresent)
  traverse getSymbol (NE.fromList [(0 :: TSSymbol) .. fromIntegral (pred count)])
  where
    getSymbol i = do
      symbolName <- fromString <$> (TS.ts_language_symbol_name language i >>= peekCString)
      t <- TS.ts_language_symbol_type language i
      let symbolNymity = if t == 0 then Named else Anonymous
      pure Symbol {..}
