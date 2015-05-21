{- http://www.codewars.com/kata/55466989aeecab5aac00003e/train/haskell (kyu 6)
The drawing below gives an idea of how to cut a given "true" rectangle into squares ("true" rectangle meaning that the two dimensions are different).

alternative text

Can you translate this drawing into an algorithm?
[][][][][]
[][][][][]
[][][][][]

5x3 grid can be cut into a 3x3, then a 2x2 then a 1x1 and another 1x1

You will be given two dimensions

a positive integer length (parameter named lng)
a positive integer width (parameter named wdth)
You will return an array with the size of each of the squares.

Examples

squaresInRect  5  3 `shouldBe` Just [3,2,1,1]
squaresInRect  3  5 `shouldBe` Just [3,2,1,1]
squaresInRect 20 14 `shouldBe` Just [14, 6, 6, 2, 2, 2]
Note: lng == wdth as a starting case would be an entirely different problem and the drawing is planned to be interpreted with lng != wdth. See kata also my other kata, Square into Squares. Protect trees!.

When the initial parameters are so that lng == wdth, the solution [lng] would be the most obvious but not in the spirit of this kata so, in that case, return None/nil/null/Nothing.

Examples:

squaresInRect 5 5 `shouldBe` Nothing


-}
module Codewars.Rectangle where

import Test.Hspec
import Test.QuickCheck
import Data.List

squaresInRect :: Integer -> Integer -> Maybe [Integer]
squaresInRect lng wdth
      | lng == wdth = Nothing
      | otherwise = Just (squaresInRect2 lng wdth)
      where
        squaresInRect2 lng wdth
              | lng == wdth = [lng]
              | otherwise = [x] ++ squaresInRect2 x (y-x)
              where ordered = sort [lng,wdth]
                    x = ordered!!0
                    y = ordered!!1



test = hspec $ do
  describe "squaresInRect" $ do
    it "should work for some examples" $ do
      squaresInRect  5  5 `shouldBe` Nothing
      squaresInRect  5  3 `shouldBe` Just [3,2,1,1]
      squaresInRect  3  5 `shouldBe` Just [3,2,1,1]
      squaresInRect 20 14 `shouldBe` Just [14, 6, 6, 2, 2, 2]

    it "should return Nothing for any square" $ property $ \(Positive x) ->
      squaresInRect x x `shouldBe` Nothing

    it "should return something for any true rectangle" $ property $ 
      \(Positive x) (Positive y) -> x /= y ==>
        squaresInRect x y `shouldSatisfy` maybe False (const True)
