module Codewars.Duplicates where
import Data.List (nub)
import Test.Hspec
import Test.QuickCheck

duplicateCount :: String -> Int
duplicateCount n = 2

count :: Char -> [Char] -> Int
count ch xs = length $ filter (notElem ch) xs

--filter (>5) [1,2,3,4,5,6,7,8]

--filter (>5) [1,2,3,4,5,6,7,8]


test = hspec $ do
  describe "duplicateCount" $ do
    it "count" $ do
       count 'a' "abcdeaaa"                     =?= 4
    it "should work for some small tests" $ do
      duplicateCount ""                         =?= 0
      duplicateCount "abcde"                    =?= 0
      duplicateCount "aabbcde"                  =?= 2
      duplicateCount "aaBbcde"                  =?= 2
      duplicateCount "Indivisibility"           =?= 1
      duplicateCount "Indivisibilities"         =?= 2
      duplicateCount ['a'..'z']                 =?= 0
      duplicateCount (['a'..'z'] ++ ['A'..'Z']) =?= 26
    it "should work for some random lists" $ do
      property $ forAll (listOf $ elements ['a'..'z']) $ \x ->
        let xs = nub x
        in duplicateCount (concatMap (replicate 2) xs) =?= length xs
  where (=?=) = shouldBe