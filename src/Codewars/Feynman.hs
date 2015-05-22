{- http://www.codewars.com/kata/551186edce486caa61000f5c/train/haskell (6 kyu)
Feynman's squares

Richard Phillips Feynman was a well-known American physicist and a recipient of the Nobel Prize in Physics. He worked in theoretical physics and pioneered the field of quantum computing.

Recently, an old farmer found some papers and notes that are believed to have belonged to Feynman. Among notes about mesons and electromagnetism, there was a napkin where he wrote a simple puzzle: "how many different squares are there in a grid of NxN squares?".

For example, when N=2, the answer is 5: the 2x2 square itself, plus the four 1x1 squares in its corners:

Task

You have to write a function

countSquares :: Integer -> Integer
that solves Feynman's question in general. The input to your function will always be a positive integer.

Examples

countSquares 1 =  1
countSquares 2 =  5
countSquares 3 = 14
(Adapted from the Sphere Online Judge problem SAMER08F by Diego Satoba)
-}

module Codewars.Feynman where

import Test.Hspec

countSquares :: Integer -> Integer
countSquares n = sum $ map (^2) [1..n]


test = hspec $ do
  describe "countSquares" $ do
    it "should work for some examples" $ do
      countSquares 1 `shouldBe` 1
      countSquares 2 `shouldBe` 5
      countSquares 3 `shouldBe` 14
      countSquares 5 `shouldBe` 55
      countSquares 8 `shouldBe` 204
      countSquares 15 `shouldBe` 1240
