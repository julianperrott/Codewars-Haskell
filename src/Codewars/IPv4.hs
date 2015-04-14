module Codewars.IPv4 where
import Test.Hspec
import Data.Int  (Int32)
import Data.Word
import Data.List

type IPString = String

int32ToIP :: Word -> IPString
int32ToIP int32 = intercalate "." $ map show $ snd $ mapAccumL (\x y -> (x `mod` (2^y) ,x `div`(2^y))) int32 [24,16,8,0]


test = hspec $
  describe "int32ToIP" $ do
    it "should work for 2154959208" $ do int32ToIP 2154959208 `shouldBe` "128.114.17.104"
    it "should work for 0" $ do int32ToIP 0          `shouldBe` "0.0.0.0"
    it "should work for 2149583361" $ do int32ToIP 2149583361 `shouldBe` "128.32.10.1"