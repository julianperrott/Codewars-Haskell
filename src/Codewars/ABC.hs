module Codewars.ABC where
import Data.List
import Test.Hspec


alphabetize :: String -> String
alphabetize = sort



test = hspec $ do
  describe "alphabetize" $ do
    it "testing 'bca'" $ shouldBe (alphabetize "bca") "abc"