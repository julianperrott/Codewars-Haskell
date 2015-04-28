module Codewars.Mean where
import           Data.List

import           Test.Hspec
import           Test.QuickCheck

mean :: (Integral a, Fractional b) => [a] -> b
mean xs =  (fromIntegral $ foldr (+) 0 xs) / (fromIntegral $ length xs)

test = hspec $ do
  describe "The mean function" $ do
    it "should work for some examples" $ do
      mean [0, 0, 0] `shouldBe` 0.0
      mean [10,4,7,5] `shouldBe` 6.5
      mean [(-4),8] `shouldBe` 2.0
