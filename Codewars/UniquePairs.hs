module Codewars.UniquePairs where
import Test.Hspec
import Test.QuickCheck

projectPartners :: Integer -> Integer
projectPartners n
    | n <= 1 = 0
    | otherwise = (n * (n-1)) `div` 2



---------------
-- Tests
---------------

test = hspec $ do
  describe "projectPartners" $ do
    it "should work for some examples" $ do
      projectPartners 2 `shouldBe`  1
      projectPartners 3 `shouldBe`  3
      projectPartners 4 `shouldBe`  6
      projectPartners 5 `shouldBe` 10

    it "should terminate in (almost) constant time" $ do
      property $ forAll (choose (10^10, 10^100)) $ \n ->
        projectPartners n `shouldSatisfy` (>0)