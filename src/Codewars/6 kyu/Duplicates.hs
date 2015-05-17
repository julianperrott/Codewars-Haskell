{- http://www.codewars.com/kata/54bf1c2cd5b56cc47f0007a1 "6 kyu","Counting Duplicates","54bf1c2cd5b56cc47f0007a1"
Count the number of Duplicates

Write a function that will return the count of distinct case-insensitive alphabetic characters that occur more than once in the given string. The given string can be assumed to contain only uppercase and lowercase alphabets.

Example

"abcde" -> 0 # no characters repeats more than once
"aabbcde" -> 2 # 'a' and 'b'
"aabbcdeB" -> 2 # 'a' and 'b'
"indivisibility" -> 1 # 'i'
"Indivisibilities" -> 2 # 'i' and 's'
-}

module Codewars.Duplicates where
import Data.List (nub)
import Test.Hspec
import Test.QuickCheck
import Data.Char
import Data.List

duplicateCount :: String -> Int
duplicateCount [] = 0
duplicateCount (x:xs)
    | length xs == length filtered = duplicateCount filtered
    | otherwise = 1 + duplicateCount filtered
    where
        filtered = filter (\y -> elem (toLower y) [toLower x] == False) xs


-- example of pattern method in where
duplicateCount2 :: String -> Int
duplicateCount2 xs = dupeCount (sort  $ map toLower xs) 0
  where
    dupeCount [] n = n
    dupeCount (x:[]) n = n
    dupeCount (x:y:xs) n = if x == y
                           then dupeCount (dropWhile (==x) xs) (n+1)
                           else dupeCount (y:xs) n



test = hspec $ do
  describe "duplicateCount" $ do
    it "should work for some small tests empty" $ do
      duplicateCount ""                         =?= 0
    it "should work for some small tests abcde" $ do
      duplicateCount "abcde"                    =?= 0
    it "should work for some small tests aabbcde" $ do
      duplicateCount "aabbcde"                  =?= 2
    it "should work for some small tests aaBbcde" $ do
      duplicateCount "aaBbcde"                  =?= 2
    it "should work for some small tests Indivisibility" $ do
      duplicateCount "Indivisibility"           =?= 1
    it "should work for some small testsIndivisibilities" $ do
      duplicateCount "Indivisibilities"         =?= 2
    it "should work for some small tests a-z" $ do
      duplicateCount ['a'..'z']                 =?= 0
    it "should work for some small tests A-Z" $ do
      duplicateCount (['a'..'z'] ++ ['A'..'Z']) =?= 26
    it "should work for some random lists" $ do
      property $ forAll (listOf $ elements ['a'..'z']) $ \x ->
        let xs = nub x
        in duplicateCount (concatMap (replicate 2) xs) =?= length xs
  where (=?=) = shouldBe
