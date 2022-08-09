{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
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
import Data.Vector (Vector)
import Foreign (Ptr)
import Foreign.C.String (peekCString)
import GHC.Generics (Generic)
import JSON qualified
import Name
import Optics
import Optics.Label ()
import Optics.TH
import TreeSitter.Language qualified as TS
import TreeSitter.Symbol (TSSymbol)

data Document = Document
  { documentLanguageName :: Name,
    documentDebugSymbols :: NonEmpty Symbol,
    documentNodeTypes :: Vector NodeType
  }

data Nature = Single | Optional | Some | Many deriving stock Show

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

data Leaf = Leaf {leafName :: Name, leafSymbolIndex :: TSSymbol}

data Token = Token Name TSSymbol

data Product = Product
  { productName :: Name,
    productFields :: [Field],
    productExtras :: Maybe Field, -- invariant: (notNull fields || isJust extras)
    productIndices :: [TSSymbol]
  } deriving stock Show

data Field = Field { fieldName :: Name, fieldNature :: Nature, fieldTypes :: Vector Name }
  deriving stock (Generic, Show)

data Symbol = Symbol {symbolName :: Name, symbolNymity :: Nymity, symbolIndex :: TSSymbol}
  deriving stock (Generic)

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
    getSymbol symbolIndex = do
      symbolName <- fromString <$> (TS.ts_language_symbol_name language symbolIndex >>= peekCString)
      t <- TS.ts_language_symbol_type language symbolIndex
      let symbolNymity = if t == 0 then Named else Anonymous
      pure Symbol {..}

makeFieldLabels ''Choice
makeFieldLabels ''Symbol
makeFieldLabels ''Product
makeFieldLabels ''Field
