{- http://www.codewars.com/kata/54521e9ec8e60bc4de000d6c

The maximum sum subarray problem consists in finding the maximum sum of a contiguous subsequence in an array or list of integers:

maxSequence([-2, 1, -3, 4, -1, 2, 1, -5, 4])
// should be 6: [4, -1, 2, 1]
Easy case is when the list is made up of only positive numbers and the maximum sum is the sum of the whole array. If the list is made up of only negative numbers, return 0 instead.

Empty list is considered to have zero greatest sum. Note that the empty list or array is also a valid sublist/subarray.
-}

module Codewars.MaxSequence where
import Test.Hspec

-- Return the greatest subarray sum within the array of integers passed in.
maxSequence :: [Int] -> Int
maxSequence [] = 0
maxSequence ar = max (listSum 0 ar) (maxSequence(tail ar))

listSum :: Int -> [Int] -> Int
listSum _ [] = 0
listSum n (x:xs) = max (n+x) (listSum (n+x) xs)


-- tails


test = hspec $ do
  describe "maxSequence" $ do
      it "arraysum" $ do  listSum 0 [1,2,3,4] `shouldBe` 10
  describe "maxSequence" $ do
    it "Should work on empty list "
     $ do maxSequence [] `shouldBe` 0
    it  "Should work for the example"
     $ do maxSequence input3 `shouldBe` expected3
  where
    input3     = [-2, 1, -3, 4, -1, 2, 1, -5, 4]
    expected3  = 6
