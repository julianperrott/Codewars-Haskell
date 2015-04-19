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
