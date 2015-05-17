{- http://www.codewars.com/kata/537baa6f8f4b300b5900106c "7 kyu","Area of a Circle","537baa6f8f4b300b5900106c"

Complete the function circleArea so that it will return the area of a circle with the given radius. Round the returned number to two decimal places (except for Haskell). If the radius is not positive or not a number, return false.

Example:

circleArea(-1485.86);    //returns false
circleArea(0);           //returns false
circleArea(43.2673);     //returns 5881.25
circleArea(68);          //returns 14526.72
circleArea("number");    //returns false
-}

module Codewars.Circle where
import Test.Hspec

circleArea :: Double -> Maybe Double
circleArea x
   | x <= 0 = Nothing
   | otherwise = Just (pi * x * x)


-- Just - Given a type t, a value of Just t is an existing value of type t, where Nothing represents a failure to reach a value, or a case where having a value would be meaningless.

---------------
-- Tests
---------------

testCircle = hspec $ do
  describe "circle" $ do
    it "should work for some simple examples" $ do
      circleArea (-1) `shouldBe` Nothing
      circleArea 0 `shouldBe` Nothing
      circleArea 1 `shouldBe` Just (    pi)
      circleArea 2 `shouldBe` Just (4 * pi)
      circleArea 3 `shouldBe` Just (9 * pi)
