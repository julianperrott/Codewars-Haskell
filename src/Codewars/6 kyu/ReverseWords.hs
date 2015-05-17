{- http://www.codewars.com/kata/51c8991dee245d7ddf00000e "6 kyu","Reversed Words","51c8991dee245d7ddf00000e"

Complete the solution so that it reverses all of the words within the string passed in.

Example:

reverseWords("The greatest victory is that which requires no battle")
// should return "battle no requires which that is victory greatest The"

-}
module Codewars.ReverseWords where

import Test.Hspec
import Data.List
import Data.List.Split

reverseWords :: String -> String
reverseWords str = concat $ intersperse " " $ reverse $ splitOn " " str





test = hspec $ do
  describe "reverseWords" $ do
    it "should work for some small examples" $ do
      reverseWords "hello world!"                 `shouldBe` "world! hello"
      reverseWords "yoda doesn't speak like this" `shouldBe` "this like speak doesn't yoda"
      reverseWords "foobar"                       `shouldBe` "foobar"
      reverseWords "row row row your boat"        `shouldBe` "boat your row row row"
