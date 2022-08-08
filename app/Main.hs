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
import Data.Aeson qualified as Aeson
import Data.Text.Builder.Linear qualified as Builder
import Data.Text qualified as Text
import Data.Text.IO qualified as Text (putStrLn)

main :: IO ()
main = do
  args <- getArgs
  when (null args) (die "Expected one argument")
  let path = head args
  -- asForeign <- Aeson.decodeFileStrict path
  let asForeign = Just ()
  case asForeign of
    Nothing -> die "Error parsing JSON, this shouldn't happen"
    Just f -> do
      -- let doc = parseDocument f
      let doc = Document [ChoiceNode (Choice "Value" ["Array", "False", "Null", "Number", "Object", "String"])]
      let asText = Builder.runBuilder (render doc)
      Text.putStrLn asText
