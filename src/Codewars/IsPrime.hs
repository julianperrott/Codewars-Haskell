module Codewars.IsPrime where
import Test.Hspec
import Data.List

isPrimeLoop :: Integer -> Integer -> Bool
isPrimeLoop n i
    | (i*i) > n = True
    | n `rem` i == 0 = False
    | n `rem` (i+2) == 0 = False
    | otherwise = isPrimeLoop n (i+6)

isPrime :: Integer -> Bool
isPrime x
    | n < 4 = n >1
    | n `rem` 2 == 0 = False
    | n `rem` 3 == 0 = False
    | otherwise = [] == (
            filter(\y -> n `mod` y == 0 || n `mod` (y+2) == 0 ) 
            $ takeWhile (\y -> y*y <= n) [5,11..]
        )
    where n = abs x

-- | otherwise = [] == (take 1 $ Data.List.filter(\x -> n `mod` x == 0 || n `mod` (x+2) == 0 ) $ takeWhile (\x -> x*x <= n) [5,11..])
-- | otherwise = isPrimeLoop n 5

-- $ takeWhile (\x -> x*x <= n) [5,11..]) -- array 5+6 < x*x

{-
for (ulong i = 5; i * i <= n; i += 6) // i squared < n
{
     if (n % i == 0 || n % (i + 2) == 0)
    {
            return false;
    }
}

    return true;
-}
---------------
-- Tests
---------------


test = hspec $
  describe "isPrime" $ do
    it "should work for some examples 0" $ do isPrime 0        `shouldBe` False 
    it "should work for some examples 1" $ do  isPrime 1        `shouldBe` False
    it "should work for some examples 2" $ do  isPrime 2        `shouldBe` True
    it "should work for some examples -2" $ do  isPrime (negate 2) `shouldBe` True
    it "should work for some examples 17" $ do  isPrime 17       `shouldBe` True
    it "should work for some examples 23423527" $ do  isPrime 23423527 `shouldBe` True