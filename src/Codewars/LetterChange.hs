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