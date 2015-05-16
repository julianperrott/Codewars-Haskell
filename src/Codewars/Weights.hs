module Codewars.Weights where
import           Test.Hspec

-- | Takes a bottleWeight and scale of
--   the bottle to its contents

contentWeight :: Int -> String -> Int
contentWeight weight str 
    | last (words str) == "larger" = weight * scale `div` (scale + 1)
    | otherwise = weight `div` (scale+1)
    where scale = read (words str !! 0)  :: Int
          
---------------
-- Tests
---------------

test = hspec $ do
  describe "contentWeight" $ do
    it "should work for larger contents" $ do
      contentWeight 120 "2 times larger" `shouldBe`  80
      contentWeight 180 "2 times larger" `shouldBe` 120
    it "should work for smaller contents" $ do
      contentWeight 120 "2 times smaller" `shouldBe`  40
      contentWeight 300 "2 times smaller" `shouldBe` 100