{- http://www.codewars.com/kata/5426d7a2c2c7784365000783

Write a function which makes a list of strings representing all of the ways you can balance n pairs of parentheses:

Example:

> balancedParens 0
[""]
> balancedParens 1
["()"]
> balancedParens 2
["()()","(())"]
> balancedParens 3
["()()()","(())()","()(())","(()())","((()))"]
-}

module Codewars.AwesomeNum where
import Test.Hspec
import Data.Char

data Answer = No | Almost | Yes  deriving (Show, Read, Eq, Ord)

isInteresting :: Integer -> [Integer] -> Answer
isInteresting x xs
    | isInterestingNum x xs = Yes
    | isInterestingNum (x+1) xs = Almost
    | isInterestingNum (x+2) xs = Almost
    | otherwise = No

isInterestingNum :: Integer -> [Integer] -> Bool
isInterestingNum x xs
    | x < 100 || x >= 1000000000 = False
    | xStr == reverse (xStr) = True
    | elem x xs = True
    | (read $ tail xStr) == 0 = True
    | take (length xStr) (drop ((digitToInt $ head $ xStr) -1) "1234567890") == xStr = True
    | take (length xStr) (drop ( 9 - (digitToInt $ head $ xStr) ) "9876543210") == xStr = True
    | otherwise = False
    where
        xStr = show x

{-
Any digit followed by all zeros: 100, 90000
The digits are sequential, incementing†: 1234 - The digits are sequential, decrementing‡: 4321
The digits are a palindrome: 1221 or 73837 (Every digit is the same number: 1111)
The digits match one of the values in the awesomePhrases array
-}

test = hspec $ do
  describe "all the same" $ do
    it "should should work for 9999" $ do isInteresting 9999 [1337, 256] `shouldBe` Yes
    it "should should work for 9998" $ do isInteresting 9998 [1337, 256] `shouldBe` Almost
  describe "incrementing sequence" $ do
    it "should should work for 1234 " $ do isInteresting  1234 [1337, 256] `shouldBe` Yes
    it "should should work for 456 " $ do isInteresting  456 [1337, 256] `shouldBe` Yes
    it "should should work for 234567890 " $ do isInteresting  234567890 [1337, 256] `shouldBe` Yes
  describe "decreasing sequence" $ do
    it "should should work for 3210 " $ do isInteresting  3210 [1337, 256] `shouldBe` Yes
    it "should should work for 4321 " $ do isInteresting  4321 [1337, 256] `shouldBe` Yes
    it "should should work for 654 " $ do isInteresting  654 [1337, 256] `shouldBe` Yes
    it "should should work for 5432109 " $ do isInteresting  543210 [1337, 256] `shouldBe` Yes
    it "should should work for 987654321 " $ do isInteresting 987654321 [1337, 256] `shouldBe` Yes
  describe "zeros" $ do
    it "should should work for zeros 100" $ do isInteresting  100 [1337, 256] `shouldBe` Yes
    it "should should work for zeros 98" $ do isInteresting  98 [1337, 256] `shouldBe` Almost
    it "should should work for zeros 90000 " $ do isInteresting  90000 [1337, 256] `shouldBe` Yes
    it "should should work for zeros 999998 " $ do isInteresting  999998 [1337, 256] `shouldBe` Almost
  describe "too low" $ do
    it "should should work for 3" $ do isInteresting     3 [1337, 256] `shouldBe` No
    it "should should work for 98" $ do isInteresting    98 [1337, 256] `shouldBe` Almost
    it "should should work for 10" $ do isInteresting    910 [1337, 256] `shouldBe` No
  describe "too big" $ do
    it "should should work for 1,000,000,000." $ do isInteresting  1000000000 [1337, 256] `shouldBe` No
  describe "leet nums" $ do
    it "should should work for 1336" $ do isInteresting  1336 [1337, 256] `shouldBe` Almost
    it "should should work for 1337" $ do isInteresting  1337 [1337, 256] `shouldBe` Yes
  describe "palindrome" $ do
    it "should should work for 11208" $ do isInteresting 11208 [1337, 256] `shouldBe` No
    it "should should work for 11209" $ do isInteresting 11209 [1337, 256] `shouldBe` Almost
    it "should should work for 11211" $ do isInteresting 11211 [1337, 256] `shouldBe` Yes
  describe "various" $ do
    it "7382" $ do isInteresting 7382 [] `shouldBe` No
    it "99919911" $ do isInteresting 99919911 [] `shouldBe` No
    it "7540" $ do isInteresting 7540 [] `shouldBe` No
    it "1590" $ do isInteresting 1590 [] `shouldBe` No
    it "11208" $ do isInteresting 11208 [] `shouldBe` No
    it "97" $ do isInteresting 97 [] `shouldBe` No
    it "100" $ do isInteresting 100 [] `shouldBe` Yes
    it "7000" $ do isInteresting 7000 [] `shouldBe` Yes
    it "800000" $ do isInteresting 800000 [] `shouldBe` Yes
    it "111" $ do isInteresting 111 [] `shouldBe` Yes
    it "444" $ do isInteresting 444 [] `shouldBe` Yes
    it "9999999" $ do isInteresting 9999999 [] `shouldBe` Yes
    it "80085" $ do isInteresting 80085 [] `shouldBe` No
    it "101" $ do isInteresting 101 [] `shouldBe` Yes
    it "11011" $ do isInteresting 11011[] `shouldBe` Yes
    it "7473747" $ do isInteresting 7473747 [] `shouldBe` Yes
    it "123" $ do isInteresting 123 [] `shouldBe` Yes
    it "1234" $ do isInteresting 1234 [] `shouldBe` Yes
    it "67890" $ do isInteresting 67890 [] `shouldBe` Yes
    it "234567890" $ do isInteresting 234567890 [] `shouldBe` Yes
    it "3210" $ do isInteresting 3210 [] `shouldBe` Yes
    it "654" $ do isInteresting 654 [] `shouldBe` Yes
    it "8765" $ do isInteresting 8765 [] `shouldBe` Yes
    it "987654321" $ do isInteresting 987654321 [] `shouldBe` Yes
    it "98" $ do isInteresting 98 [] `shouldBe` Almost
    it "99" $ do isInteresting 99 [] `shouldBe` Almost
    it "109" $ do isInteresting 109 [] `shouldBe` Almost
    it "987654320" $ do isInteresting 987654320 [] `shouldBe` Almost
