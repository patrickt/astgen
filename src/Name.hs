{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
module Name
  ( Name (..)
  , asConstructor
  , escaped
  , toPascalCase
  , camelCase
  ) where
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Coerce
import TreeSitter.Symbol (toHaskellPascalCaseIdentifier, toHaskellCamelCaseIdentifier)


newtype Name = Name Text
  deriving stock Eq
  deriving newtype (IsString)

instance Show Name where
  show = Text.unpack . coerce

asConstructor :: Name -> Text
asConstructor (Name n) = Text.toTitle . Text.dropWhile (== '_') $ n

escaped :: Name -> Text
escaped (Name (Text.unpack -> t)) = Text.pack done
  where
    done = t >>= (\c -> if c == '"' then "\\\"" else [c])

toPascalCase :: Name -> Text
toPascalCase = Text.pack . toHaskellPascalCaseIdentifier . Text.unpack . coerce

camelCase :: Name -> Text
camelCase = Text.pack . toHaskellCamelCaseIdentifier . Text.unpack . coerce
