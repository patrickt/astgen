{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Name
  ( Name (..)
  , asConstructor
  ) where
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Coerce


newtype Name = Name Text
  deriving stock Eq
  deriving newtype (IsString)

instance Show Name where
  show = Text.unpack . coerce

asConstructor :: Name -> Text
asConstructor (Name n) = Text.toTitle . Text.dropWhile (== '_') $ n
