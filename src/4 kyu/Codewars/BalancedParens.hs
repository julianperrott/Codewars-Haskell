{- http://www.codewars.com/kata/5426d7a2c2c7784365000783 "4 kyu","All Balanced Parentheses","5426d7a2c2c7784365000783"

Write a function which makes a list of strings representing all of the ways you can balance n pairs of parentheses:

Example:

> balancedParens 0
[""]
> balancedParens 1
["()"]
> balancedParens 2
["()()","(())"]
> balancedParens 3
["()()()","(())()","()(())","(()())","((()))"]

-}

module Codewars.BalancedParens where
import Test.Hspec
import Data.List (sort)

balancedParens :: Int -> [String]
balancedParens n = balancedParenBuilder n ["("]

balancedParenBuilder :: Int -> [String] -> [String]
balancedParenBuilder n xs
    | n == 0 = [""]
    | length (xs !! 0) == (n*2)-1 = map (++")") xs  -- params done
    | otherwise = balancedParenBuilder n $ addOpen ++ addClose
    where addOpen = [ i ++ "(" | i <- xs, isValid n (i ++ "(")]
          addClose = [ i ++ ")" | i <- xs, isValid n (i ++ ")")]

isValid :: Int -> String -> Bool
isValid n str = opCnt >= clCnt && opCnt <= n
    where opCnt = length $ filter (=='(') str
          clCnt = (length str) - opCnt


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

