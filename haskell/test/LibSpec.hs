module LibSpec where

import Test.Hspec

import Lib

spec :: Spec
spec = do
  describe "combinations" $ do
   it "return 3 for 2 out of 3" $ do
     length (combinations 2 ["a", "b", "c"]) `shouldBe` 3
