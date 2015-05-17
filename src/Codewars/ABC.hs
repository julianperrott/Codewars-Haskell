{- http://www.codewars.com/kata/54ba504e2804ced78a000170

Reorganize into alphabetical order a nonempty string consisting only of lower-case English letters.

alphabetize "bca" == "abc"

-}

module Codewars.ABC where
import Data.List
import Test.Hspec


alphabetize :: String -> String
alphabetize = sort



test = hspec $ do
  describe "alphabetize" $ do
    it "testing 'bca'" $ shouldBe (alphabetize "bca") "abc"
