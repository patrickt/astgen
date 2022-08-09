{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
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
import TreeSitter.JSON (tree_sitter_json)
import Data.Text.IO qualified as Text
import qualified TreeSitter.Language as TS
import Foreign (Ptr)
import Data.Char
import System.IO (openTempFile, hClose)
import System.Process (system)

getLanguage :: String -> Ptr TS.Language
getLanguage = \case
  "json" -> tree_sitter_json
  x -> error ("unrecognized language " <> x)

main :: IO ()
main = do
  [langname, path] <- getArgs
  let lang = getLanguage (map toLower langname)
  asForeign <- Aeson.decodeFileStrict path
  case asForeign of
    Nothing -> die "Error parsing JSON, this shouldn't happen"
    Just f -> do
      doc <- convert lang langname f
      case doc of
        Left err -> die ("Error: " <> show err)
        Right val -> do
          (fp, h) <- openTempFile "/tmp" "astgen.hs"
          let asText = Builder.runBuilder (render val)
          Text.hPutStrLn h asText
          hClose h
          void do
            system ("ormolu -i " <> fp)
            system ("cat " <> fp)
