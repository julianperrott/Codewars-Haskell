{- http://www.codewars.com/kata/52705ed65de62b733f000064 "7 kyu","Return a sorted list of objects","52705ed65de62b733f000064"

You'll be passed an array of objects - you must sort them in descending order based on the value of an arbitrarily specified property. For example, when sorted by a, this:

[
  {a: 1, b: 3},
  {a: 3, b: 2},
  {a: 2, b: 40},
  {a: 4, b: 12}
]
should return:

[
  {a: 4, b: 12},
  {a: 3, b: 2},
  {a: 2, b: 40},
  {a: 1, b: 3}
]
your function must take the form function sortList (sortBy, list)

The values will always be numbers, and the properties will always exist.
-}

module Codewars.Sorted where
import Test.Hspec

sortList :: Ord b => (a -> b) -> [a] -> [a]
sortList f = error "TODO: sortList"

---------------
-- Tests
---------------

test = hspec $ do
    describe "sortList" $ do
      it "should work on some examples" $ do
        sortList fst (zip [1..10] [20,19..]) `shouldBe` zip [1..10]  [20,19..]
        sortList snd (zip [1..10] [20,19..]) `shouldBe` zip [10,9..1] [11..20]
