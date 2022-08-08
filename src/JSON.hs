{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module JSON (NodeTypes (NodeTypes), NodeInfo (NodeInfo), FieldInfo (FieldInfo), Node (Node)) where

import Data.Vector (Vector)
import Data.Aeson (FromJSON (..), (.:), (.:?), withObject)
import Data.Text (Text)
import Data.Aeson qualified as Aeson
import Data.HashMap.Strict (HashMap)
import GHC.Generics (Generic)
import Optics

newtype NodeTypes = NodeTypes (Vector NodeInfo)
  deriving stock Show
  deriving newtype FromJSON

data NodeInfo = NodeInfo
  {
    nodeInfoType :: Text,
    nodeInfoNamed :: Bool,
    nodeInfoFields :: Maybe (HashMap Text FieldInfo),
    nodeInfoChildren :: Maybe FieldInfo,
    nodeInfoSubtypes :: Maybe (Vector Node)
  } deriving (Show, Generic)

instance FromJSON NodeInfo where
  parseJSON = withObject "NodeInfo" \o -> do
    nodeInfoType <- o .: "type"
    nodeInfoNamed <- o .: "named"
    nodeInfoFields <- o .:? "fields"
    nodeInfoChildren <- o .:? "children"
    nodeInfoSubtypes <- o .:? "subtypes"
    pure NodeInfo{..}

data FieldInfo = FieldInfo
  {
    fieldInfoMultiple :: Bool,
    fieldInfoRequired :: Bool,
    fieldInfoTypes :: Vector Node
  } deriving (Show, Generic)

instance FromJSON FieldInfo where
  parseJSON = withObject "FieldInfo" \o -> do
    fieldInfoMultiple <- o .: "multiple"
    fieldInfoRequired <- o .: "required"
    fieldInfoTypes <- o .: "types"
    pure FieldInfo{..}

data Node = Node
  {
    nodeType :: Text,
    nodeNamed :: Bool
  } deriving (Show, Generic)

instance FromJSON Node where
  parseJSON = withObject "Node" \o -> do
    nodeType <- o .: "type"
    nodeNamed <- o .: "named"
    pure Node{..}

makeFieldLabels ''NodeInfo
makeFieldLabels ''FieldInfo
makeFieldLabels ''Node

someFunc :: IO ()
someFunc = putStrLn "someFunc"
