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