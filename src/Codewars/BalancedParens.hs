module Codewars.BalancedParens where
import Test.Hspec
import Data.List (sort)

balancedParens :: Int -> [String]
balancedParens n = balancedParenBuilder n ["("]

balancedParenBuilder :: Int -> [String] -> [String]
balancedParenBuilder n xs
    | n == 0 = [""]
    | length xs == 0 = ["EEK"]
    | length (xs !! 0) == (n*2) = xs -- params done
    | otherwise = balancedParenBuilder n [ i ++ "(" | i <- xs, isValid n (i++"(")] ++ [ i ++ ")" | i <- xs, isValid n (i++")")]

isValid :: Int -> String -> Bool
isValid n str = True


test = hspec $ do
  describe "Basic Tests" $ do
    it "n = 0" $ do
      (sort . balancedParens) 0 `shouldBe` [""] --0
    it "n = 1" $ do
      (sort . balancedParens) 1 `shouldBe` ["()"] -- 01 (n * 2)
    it "n = 2" $ do
      (sort . balancedParens) 2 `shouldBe` ["(())","()()"] -- 0011 0101 (4)
    it "n = 3" $ do
      (sort . balancedParens) 3 `shouldBe` ["((()))","(()())","(())()","()(())","()()()"]  -- 000111 001011 001101 010011 010101 --(8)
    it "n = 4" $ do
      (sort . balancedParens) 4 `shouldBe` ["(((())))","((()()))","((())())","((()))()","(()(()))","(()()())","(()())()","(())(())","(())()()","()((()))","()(()())","()(())()","()()(())","()()()()"]

