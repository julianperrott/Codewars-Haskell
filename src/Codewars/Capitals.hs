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