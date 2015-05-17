{- http://www.codewars.com/kata/521ef596c106a935c0000519

We need prime numbers and we need them now!

Write a method that takes a maximum bound and returns all primes starting with 0 up-to and including the maximum bound.

For example:

prime(11);
Should return an array that looks like this:

[2,3,5,7,11]
-}

module Codewars.PrimeTime where
import Test.Hspec
import Data.List


prime :: Int -> [Int]
prime n
    | n < 2 = []
    | otherwise = getPrimes  [2] [3,5..n]

getPrimes :: [Int] -> [Int] -> [Int]
getPrimes primes [] = primes
getPrimes primes candidates = getPrimes (if isPrime then (primes ++ [head candidates]) else primes) $ tail candidates
    where isPrime = (take 1 $ filter(\x -> (head candidates) `mod` x == 0) $ takeWhile (\x -> x*x <= (head candidates)) primes ) == []



test = hspec $ do
  describe "prime" $ do
    it "should work for some examples 0" $ do
      prime    0 `shouldBe` []
    it "should work for some examples 1" $ do
      prime    1 `shouldBe` []
    it "should work for some examples 2" $ do
      prime    2 `shouldBe` [2]
    it "should work for some examples 23" $ do
      prime   23 `shouldBe` [2,3,5,7,11,13,17,19,23]
