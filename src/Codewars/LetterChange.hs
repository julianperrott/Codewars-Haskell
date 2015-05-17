{- http://www.codewars.com/kata/5530b10808541c24330000b4

Welcome to this Kata. In this Kata you will be given a string. Your task is to replace every character with the letter following it in the alphabet (for example, "b" should be "c", "z" should be "a" and capital "Z" should be "A").

The test cases would not have any special symbols or numbers but it will have spaces. And the upper and lower cases should be retained in your output.

For Example:

letterChange('Lorem Ipsum')    // return Mpsfn Jqtvn
-}

module Codewars.LetterChange where
import Test.Hspec
import Test.QuickCheck
import Data.Char

-- | Takes a string that contains only ASCII letters
--   and strings and shifts them according to the
--   description.
letterChange :: String -> String
letterChange = map nextChar
    where nextChar c
              | c == 'Z' = 'A'
              | c == 'z' = 'a'
              | c == ' ' = ' '
              | otherwise = chr ((ord c) +1)



test = hspec $ do
  describe "letterChange" $ do
    it "should work for some examples" $ do
      letterChange "JavaScript"  `shouldBe` "KbwbTdsjqu"
      letterChange "Lorem Ipsum" `shouldBe` "Mpsfn Jqtvn"

    it "should work for the corner cases" $ do
      letterChange "Z" `shouldBe` "A"
      letterChange "z" `shouldBe` "a"
