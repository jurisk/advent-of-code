module Test.Year2016.Day01 where

import Prelude

import Effect (Effect)
import Test.Assert (assertEqual)
import Year2016.Day01 (part1, part2, readRealInput)
import Effect.Console (log)
import Data.Maybe (Maybe(..))

main :: Effect Unit
main = do
  log "2016-01 part 1"

  assertEqual {
    actual: part1 "R2, L3",
    expected: Just 5
  }

  assertEqual {
    actual: part1 "R2, R2, R2",
    expected: Just 2
  }

  assertEqual {
    actual: part1 "R5, L5, R5, R3",
    expected: Just 12
  }

  realInput <- readRealInput
  assertEqual {
    actual: part1 realInput,
    expected: Just 271
  }

  log "2016-01 part 2"
  assertEqual {
    actual: part2 "R8, R4, R4, R8",
    expected: Just 4
  }

  assertEqual {
    actual: part2 realInput,
    expected: Just 153
  }
