module Year2021.Day05.SolutionSpec (main, spec) where

import Test.Hspec

import Year2021.Day05.Solution

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Part 1" $ do

    it "works with test data" $ do
      contents <- readFile "./test/Year2021/Day05/test.txt"
      let result = task_1 contents
      result `shouldBe` Right 5

    it "works with real data" $ do
      contents <- readFile "./test/Year2021/Day05/real.txt"
      let result = task_1 contents
      result `shouldBe` Right 5147

  describe "Part 2" $ do

    it "works with test data" $ do
      contents <- readFile "./test/Year2021/Day05/test.txt"
      let result = task_2 contents
      result `shouldBe` Right 12

    it "works with real data" $ do
      contents <- readFile "./test/Year2021/Day05/real.txt"
      let result = task_2 contents
      result `shouldBe` Right 16925
