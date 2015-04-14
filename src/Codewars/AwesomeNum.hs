module Codewars.AwesomeNum where
import Test.Hspec

data Answer = No | Almost | Yes deriving (Show, Read, Eq, Ord)

isInteresting :: Integer -> [Integer] -> Answer
isInteresting x xs
    | x < 99 || x >= 1000000000 = No
    | isInterestingNum x xs = Yes
    | isInterestingNum (x+1) xs = Almost
    | isInterestingNum (x+2) xs = Almost
    | otherwise = No

isInterestingNum :: Integer -> [Integer] -> Bool
isInterestingNum x xs
    | show x == reverse (show x) = True
    | elem x xs = True
    | read (tail (show x)) == 0 = True
    | otherwise = False

{-
Any digit followed by all zeros: 100, 90000
The digits are sequential, incementing†: 1234 - The digits are sequential, decrementing‡: 4321
The digits are a palindrome: 1221 or 73837 (Every digit is the same number: 1111)
The digits match one of the values in the awesomePhrases array
-}

test = hspec $ do
  describe "incrementing sequence" $ do
    it "should should work for 1234 " $ do isInteresting  1234 [1337, 256] `shouldBe` Yes
    it "should should work for 456 " $ do isInteresting  456 [1337, 256] `shouldBe` Yes
    it "should should work for 234567890 " $ do isInteresting  234567890 [1337, 256] `shouldBe` Yes
  describe "zeros" $ do
    it "should should work for zeros 100 " $ do isInteresting  100 [1337, 256] `shouldBe` Yes
    it "should should work for zeros 98" $ do isInteresting  98 [1337, 256] `shouldBe` Almost
    it "should should work for zeros 90000 " $ do isInteresting  90000 [1337, 256] `shouldBe` Yes
  describe "too low" $ do
    it "should should work for 3" $ do isInteresting     3 [1337, 256] `shouldBe` No
    it "should should work for 98" $ do isInteresting    98 [1337, 256] `shouldBe` No
    it "should should work for 10" $ do isInteresting    910 [1337, 256] `shouldBe` No
  describe "too big" $ do
    it "should should work for 1,000,000,000." $ do isInteresting  1000000000 [1337, 256] `shouldBe` No
  describe "leet nums" $ do
    it "should should work for 1336" $ do isInteresting  1336 [1337, 256] `shouldBe` Almost
    it "should should work for 1337" $ do isInteresting  1337 [1337, 256] `shouldBe` Yes
  describe "palindrome" $ do
    it "should should work for 11208" $ do isInteresting 11208 [1337, 256] `shouldBe` No
    it "should should work for 11209" $ do isInteresting 11209 [1337, 256] `shouldBe` Almost
    it "should should work for 11211 Pal" $ do isInteresting 11211 [1337, 256] `shouldBe` Yes
  describe "all the same" $ do
    it "should should work for 9999" $ do isInteresting 9999 [1337, 256] `shouldBe` Yes
    it "should should work for 9998" $ do isInteresting 9999 [1337, 256] `shouldBe` Almost
    

