{- http://www.codewars.com/kata/52ea928a1ef5cfec800003ee "6 kyu","IPv4 to int32","52ea928a1ef5cfec800003ee"
Take the following IPv4 address: 128.32.10.1 This address has 4 octets where each octet is a single byte (or 8 bits).

1st octet 128 has the binary representation: 10000000
2nd octet 32 has the binary representation: 00100000
3rd octet 10 has the binary representation: 00001010
4th octet 1 has the binary representation: 00000001
So 128.32.10.1 == 10000000.00100000.00001010.00000001

Because the above IP address has 32 bits, we can represent it as the 32 bit number: 2149583361.

Write a function ip_to_int32(ip) ( JS: ipToInt32(ip) ) that takes an IPv4 address and returns a 32 bit number.

  ipToInt32("128.32.10.1") => 2149583361
-}

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
