{- http://www.codewars.com/kata/51b6249c4612257ac0000005

Create a function that takes a Roman numeral as its argument and returns its value as a numeric decimal integer. You don't need to validate the form of the Roman numeral.

Modern Roman numerals are written by expressing each decimal digit of the number to be encoded separately, starting with the leftmost digit and skipping any 0s. So 1990 is rendered "MCMXC" (1000 = M, 900 = CM, 90 = XC) and 2008 is rendered "MMVIII" (2000 = MM, 8 = VIII). The Roman numeral for 1666, "MDCLXVI", uses each letter in descending order.

Example:

solution('XXI'); // should return 21

-}
module Codewars.Roman where
import Test.Hspec
import Data.List

solution :: String -> Int
solution str = sum $ snd $ mapAccumL (\ac x -> (x ,if x > ac then x-ac-ac else x )) 0 $ map romantoInt str
    where romantoInt c
            | c == 'M' = 1000
            | c == 'D' = 500
            | c == 'C' = 100
            | c == 'L' = 50
            | c == 'X' = 10
            | c == 'V' = 5
            | c == 'I' = 1
            | otherwise = 0

test = hspec $ do
  describe "Converted" $ do
    it "XXI" $ do solution "XXI" `shouldBe` 21
    it "MMVIII" $ do solution "MMVIII" `shouldBe` 2008
    it "MDCLXVI" $ do solution "MDCLXVI" `shouldBe` 1666
    it "MCMXC" $ do solution "MCMXC" `shouldBe` 1990

{-
M = 1000
D = 500
C = 100
L = 50
X = 10
V = 5
-}
