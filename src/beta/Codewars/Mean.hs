{- http://www.codewars.com/kata/54b709b811ac249fdb000296 "Beta","That's mean","54b709b811ac249fdb000296"

Calculate the mean of a nonempty list of integers. The integers in the list can be positive or negative. To keep Haskell's types happy, use fromIntegral.

mean [0, 0, 0]  == 0.0
mean [10,4,7,5] == 6.5
mean [(-4),8]   == 2.0
-}

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
