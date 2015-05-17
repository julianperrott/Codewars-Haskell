{- http://www.codewars.com/kata/5426d7a2c2c7784365000783

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

module Codewars.Parentheses where
import Test.Hspec
import Data.List

validParentheses :: String -> Bool
validParentheses str = hasNoNegativeValues && lastValueIsZero
        where sumList = snd $ mapAccumL (\x y -> (x+y,x+y)) 0 [if x=='(' then 1 else -1  |x <- str]
              hasNoNegativeValues = length (filter (<0) sumList) == 0
              lastValueIsZero = (last sumList) == 0




test = hspec $ do
  describe "validParentheses" $ do
    it "should work for ()" $ do validParentheses "()" `shouldBe` True
    it "should work for ()()" $ do validParentheses "()()" `shouldBe` True
    it "should work for (())" $ do validParentheses "(())" `shouldBe` True
    it "should work for )(" $ do validParentheses ")(" `shouldBe` False
    it "should work for )" $ do validParentheses ")"  `shouldBe` False
    it "should work for )" $ do validParentheses "("  `shouldBe` False
    it "should work for (())((()())())" $ do validParentheses "(())((()())())"  `shouldBe` True
