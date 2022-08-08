{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}
module Native (module Native) where

import Control.Effect.Error
import Data.Bool (bool)
import Data.Coerce
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as Text
import qualified JSON
import Control.Monad
import Optics
import Optics.TH
import Optics.Label ()
import Data.Maybe
import Data.String (IsString)
import Data.Word (Word16)

newtype Document = Document [NodeType]

parseDocument :: JSON.NodeTypes -> Document
parseDocument = error "TODO"

newtype Name = Name Text
  deriving newtype (IsString)

instance Show Name where
  show = Text.unpack . coerce

data Nature = Single | Optional | Some | Many

data NodeType
  = ChoiceNode Choice
  | LeafNode Leaf
  | TokenNode Token
  | ProductNode Product

data Choice = Choice
  {
    choiceName :: Name,
    choiceSubtypes :: NonEmpty Name
  }

newtype Leaf = Leaf Name

data Token = Token Name Word16

data Product = Product Name Nature [Name]

makeFieldLabels ''Choice

data ParseError = NoSubtypesPresent | UnexpectedAnonymousSubtype

parseChoice :: (MonadFail m, Has (Throw ParseError) sig m) => JSON.NodeInfo -> m NodeType
parseChoice j =
  if
    | isJust (j ^. #subtypes) -> do
        case NE.nonEmpty (j ^.. #subtypes % folded % folded % #type) of
          Nothing -> throwError NoSubtypesPresent
          Just l  -> do
            when (anyOf (#subtypes % folded % folded % #named) not j) (throwError UnexpectedAnonymousSubtype)
            pure . ChoiceNode $ Choice { choiceName = j ^. #type % to Name, choiceSubtypes = coerce l }
    | otherwise -> fail "unrecognized"
