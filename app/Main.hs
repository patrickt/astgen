{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module Main where

import System.Exit (die)
import System.Environment (getArgs)
import Control.Monad
import JSON qualified
import Native
import Render
import Convert
import Data.Aeson qualified as Aeson
import Data.Text.Builder.Linear qualified as Builder
import Data.Text qualified as Text
import Data.Text.IO qualified as Text (putStrLn)

main :: IO ()
main = do
  args <- getArgs
  when (null args) (die "Expected one argument")
  let path = head args
  asForeign <- Aeson.decodeFileStrict path
  case asForeign of
    Nothing -> die "Error parsing JSON, this shouldn't happen"
    Just f -> do
      doc <- convert "JSON" f
      case doc of
        Left err -> die ("Error: " <> show err)
        Right val -> do
          let asText = Builder.runBuilder (render val)
          Text.putStrLn asText
