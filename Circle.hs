module Circle where
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