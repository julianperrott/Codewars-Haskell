module ListOps where
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

testListOps = hspec $ do
    it "Example tests" $ do
      ListOps.head [1,5] `shouldBe` 1
      ListOps.head [3,2,4,5,5] `shouldBe` 3
      ListOps.tail [3,2,4,5,5] `shouldBe` [2,4,5,5]
      ListOps.tail [1] `shouldBe` []
      ListOps.init [1,5,7,9] `shouldBe` [1,5,7]
      ListOps.last [1,5] `shouldBe` 5
      ListOps.last [1,5,78] `shouldBe` 78
