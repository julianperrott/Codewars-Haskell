{-http://www.codewars.com/kata/54592a5052756d5c5d0009c3 "7 kyu","Head, Tail, Init and Last","54592a5052756d5c5d0009c3"

Haskell has some useful functions for dealing with lists:

$ ghci
GHCi, version 7.6.3: http://www.haskell.org/ghc/  :? for help
λ head [1,2,3,4,5]
1
λ tail [1,2,3,4,5]
[2,3,4,5]
λ init [1,2,3,4,5]
[1,2,3,4]
λ last [1,2,3,4,5]
5
Your job is to implement these functions in your given language. Make sure it doesn't edit the array; that would cause problems! Here's a cheat sheet:

| HEAD | <----------- TAIL ------------> |
[  1,  2,  3,  4,  5,  6,  7,  8,  9,  10]
| <----------- INIT ------------> | LAST |

head [x] = x
tail [x] = []
init [x] = []
last [x] = x
Here's how I expect the functions to be called in your language:

head([1,2,3,4,5]); => 1
tail([1,2,3,4,5]); => [2,3,4,5]
Most tests consist of 100 randomly generated arrays, each with four tests, one for each operation. There are 400 tests overall. No empty arrays will be given. Haskell has QuickCheck tests

PLEASE NOTE: Clojure's last function shall be called last_ to prevent name clashes.
-}

module Codewars.ListOps where
import Test.Hspec
import Test.QuickCheck
import Data.List
import Prelude hiding (head, tail, init, last)

-- https://hackage.haskell.org/package/base-4.7.0.2/docs/Data-List.html
-- https://wiki.haskell.org/How_to_work_on_lists

-- Extract the first element of a list, which must be non-empty.
-- !! = Get the Nth element out of a list.
head :: [a] -> a
head xs = xs !! 0

-- Extract the elements after the head of a list, which must be non-empty.
-- drop - Delete the first N elements from a list.
tail :: [a] -> [a]
tail xs = drop 1 xs

-- Return all the elements of a list except the last one. The list must be non-empty.
-- take - Make a new list containing just the first N elements from an existing list.
init :: [a] -> [a]
init xs = take ((length xs)-1) xs

-- Extract the last element of a list, which must be finite and non-empty.
last :: [a] -> a
last xs = xs !! ((length xs)-1)

---------------
-- Tests
---------------

testListOps = hspec $ do
    it "Example tests" $ do
      Codewars.ListOps.head [1,5] `shouldBe` 1
      Codewars.ListOps.head [3,2,4,5,5] `shouldBe` 3
      Codewars.ListOps.tail [3,2,4,5,5] `shouldBe` [2,4,5,5]
      Codewars.ListOps.tail [1] `shouldBe` []
      Codewars.ListOps.init [1,5,7,9] `shouldBe` [1,5,7]
      Codewars.ListOps.last [1,5] `shouldBe` 5
      Codewars.ListOps.last [1,5,78] `shouldBe` 78
