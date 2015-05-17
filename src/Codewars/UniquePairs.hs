{- http://www.codewars.com/kata/540c013634e6bac0350000a5

Mrs. Frizzle is beginning to plan lessons for her science class next semester, and wants to encourage friendship amongst her students. To accomplish her goal, Mrs. Frizzle will ensure each student has a chance to partner with every other student in the class in a series of science projects.

Mrs. Frizzle does not know who will be in her class next semester, but she does know she will have n students total in her class.

Specifications

Create the function projectPartners with the parameter n representing the number of students in Mrs. Frizzle's class. The function should return the total number of unique pairs she can make with n students.

projectPartners(2)
  --> returns 1

projectPartners(4)
  --> returns 6
Remarks

your solution should not contain any arrays or objects that are used similar to an array. Note that Haskell will use rather large numbers, such as 10^40, so any list-based solution will autmatically fail the test.
your function will only recieve a single number that is greater than or equal to 2 -- you do not need to worry about input validation.
-}

module Codewars.UniquePairs where
import Test.Hspec
import Test.QuickCheck

projectPartners :: Integer -> Integer
projectPartners n
    | n <= 1 = 0
    | otherwise = (n * (n-1)) `div` 2



---------------
-- Tests
---------------

test = hspec $ do
  describe "projectPartners" $ do
    it "should work for some examples" $ do
      projectPartners 2 `shouldBe`  1
      projectPartners 3 `shouldBe`  3
      projectPartners 4 `shouldBe`  6
      projectPartners 5 `shouldBe` 10

    it "should terminate in (almost) constant time" $ do
      property $ forAll (choose (10^10, 10^100)) $ \n ->
        projectPartners n `shouldSatisfy` (>0)
