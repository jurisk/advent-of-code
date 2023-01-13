module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)
import Test.Year2016.Day01 (main) as Year2016Day01

-- TODO: Consider moving to https://github.com/purescript-spec/purescript-spec
main :: Effect Unit
main = do
  log "Running tests..."
  Year2016Day01.main
