module SequenceSum where
import Test.Hspec

sumIt :: Int -> Int
sumIt n = sum[0..n]

sumOfN :: Int -> [Int]
sumOfN n
    | n < 0 = map negate (sumOfN (abs(n)))
    | otherwise = map sumIt [0..n]

testSequenceSum = hspec $ do
  describe "sumOfN" $ do
    it "should work for some examples" $ do
      sumOfN 3    `shouldBe` [0, 1, 3, 6]
      sumOfN 1    `shouldBe` [0, 1]
      sumOfN 0    `shouldBe` [0]
      sumOfN (-4) `shouldBe` [0, -1, -3, -6, -10]
