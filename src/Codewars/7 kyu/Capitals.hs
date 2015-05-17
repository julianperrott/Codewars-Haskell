{- http://www.codewars.com/kata/539ee3b6757843632d00026b "7 kyu","Find the capitals","539ee3b6757843632d00026b"

Instructions

Write a function that takes a single string (word) as argument. The function must return an ordered list containing the indexes of all capital letters in the string.

Example

Test.assertSimilar( capitals('CodEWaRs'), [0,3,4,6] );
-}

module Codewars.Capitals where
import Test.Hspec
import Data.Char

capitals :: String -> [Int]
--capitals str = map snd (filter (isUpper . fst) (zip str [0..]))

capitals = map fst .         -- get first value
    filter (isUpper . snd) . -- filter tuple array
    zip [0..]                -- make tuple array

---------------
-- Tests
---------------

test = hspec $ do
  describe "capitals" $ do
    it "should work for some small examples" $ do
      capitals ""         `shouldBe` []
      capitals "CodEWaRs" `shouldBe` [0,3,4,6]
      capitals "aAbB"     `shouldBe` [1,3]
      capitals "4ysdf4"   `shouldBe` []
      capitals "ABCDEF"   `shouldBe` [0..5]
