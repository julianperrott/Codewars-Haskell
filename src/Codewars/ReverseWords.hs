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