import Test.Hspec
import Circle

main = hspec $ do
  describe "circle" $ do
    it "should work for some simple examples" $ do
      circleArea (-1) `shouldBe` Nothing
      circleArea 0 `shouldBe` Nothing
      circleArea 1 `shouldBe` Just (    pi)
      circleArea 2 `shouldBe` Just (4 * pi)
      circleArea 3 `shouldBe` Just (9 * pi)