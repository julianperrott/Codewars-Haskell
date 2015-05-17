{- http://www.codewars.com/kata/5274e122fc75c0943d000148

Finish the solution so that it takes an input 'n' (integer) and returns a string that is the decimal representation of the number grouped by commas after every 3 digits.

Assume: 0 <= n < 1000000000

       1  ->           "1"
      10  ->          "10"
     100  ->         "100"
    1000  ->       "1,000"
   10000  ->      "10,000"
  100000  ->     "100,000"
 1000000  ->   "1,000,000"
35235235  ->  "35,235,235"
-}

module Codewars.Commas where
import Test.Hspec
import Test.QuickCheck

import Data.List
import Data.List.Split

groupByCommas :: Int -> String
groupByCommas n
    | n < 1000 = show n
    | otherwise = (groupByCommas (n `quot` 1000)) ++ "," ++ (reverse $ take 3 $ reverse $ show n)

-- groupByCommas = reverse .          intercalate "," . chunksOf 3 . reverse . show
-- groupByCommas = reverse . concat . intersperse "," . chunksOf 3 . reverse . show

-- intercalate xs xss is equivalent to (concat (intersperse xs xss)). It inserts the list xs in between the lists in xss and concatenates the result.

{-
The intersperse function takes an element and a list and `intersperses' that element between the elements of the list.
For example, > intersperse ',' "abcde" == "a,b,c,d,e"
-}

{-
O(n) Splits a Text into components of length k. The last element may be shorter than the other chunks, depending on the length of the input.
Examples:
> chunksOf 3 "foobarbaz" == ["foo","bar","baz"]
> chunksOf 4 "haskell.org" == ["hask","ell.","org"]
-}

---------------
-- Tests
---------------

test = hspec $ do
  describe "groupByCommas" $ do
    it "should work for some examples 1" $ do
      groupByCommas        1 `shouldBe`          "1"
    it "should work for some examples 10" $ do
      groupByCommas       10 `shouldBe`         "10"
    it "should work for some examples 1000" $ do
      groupByCommas     1000 `shouldBe`      "1,000"
    it "should work for some examples 35235235" $ do
      groupByCommas 35235235 `shouldBe` "35,235,235"

    it "should have the correct comma count" $ do
      property $ forAll (arbitrary `suchThat` (>=0) ) $ \x ->
        let commas = Prelude.length . Prelude.filter (==',') . groupByCommas $ x
        in  commas `shouldBe` ((max 0 . floor . logBase 10 . fromIntegral $ x) `quot` 3)
