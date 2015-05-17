{- http://www.codewars.com/kata/5412509bd436bd33920011bc "7 kyu","Credit Card Mask","5412509bd436bd33920011bc"

Usually when you buy something, you're asked whether your credit card number, phone number or answer to your most secret question is still correct. However, since someone could look over your shoulder, you don't want that shown on your screen. Instead, we mask it.

Your task is to write a function maskify, which changes all but the last four characters into '#'.

Examples

maskify("4556364607935616") == "############5616"
maskify(     "64607935616") ==      "#######5616"
maskify(               "1") ==                "1"
maskify(                "") ==                 ""

// "What was the name of your first pet?"
maskify("Skippy")                                   == "##ippy"
maskify("Nananananananananananananananana Batman!") == "####################################man!"
-}

module Codewars.Maskify where
import Data.List
import Test.Hspec
import Test.QuickCheck

len :: [char] -> Int
len [] = 0
len (h:t) = 1 + len t


maskify :: String -> String
maskify str = replicate ((length str) -4) '#' ++ reverse (take 4 (reverse str))

maskifyV2 :: String -> String
maskifyV2 str
    | (length str) < 5 = str
    | otherwise = ['#'] ++ maskifyV2 (tail str)


-- replicate - replicate n x is a list of length n with x the value of every element. It is an instance of the more general Data.List.genericReplicate, in which n may be of any ...
--lenghth - O(n) Returns the number of characters in a Text. Subject to fusion.
-- tail -
-- reverse
-- tak

---------------
-- Tests
---------------
testMaskify = hspec $ do
  describe "maskify" $ do
    it "should mask the credit card"    $ maskify "4556364607935616" `shouldBe` "############5616"
    it "should mask another number"     $ maskify "64607935616" `shouldBe` "#######5616"
    it "should mask a short number"     $ maskify "616" `shouldBe` "616"
    it "should mask a single character" $ maskify "1" `shouldBe` "1"
    it "should mask an empty string"    $ maskify "" `shouldBe` ""
    it "should mask your pet"           $ maskify "Skippy" `shouldBe` "##ippy"
    it "should mask batman"             $
      maskify "Nananananananananananananananana Batman!" `shouldBe` "####################################man!"
    it "shouldn't change the length" $
      property $ \x ->
        length x == length (maskify x)

