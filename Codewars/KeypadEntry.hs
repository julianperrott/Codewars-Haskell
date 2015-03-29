module Codewars.KeypadEntry where
import Test.Hspec
import Data.List
import Data.Maybe
import Data.Char

keyPresses :: Char -> Int
keyPresses c
    | length keysets > 0 = fromJust (elemIndex c (head keysets)) + 1
    | otherwise = 0
    where keysets =filter (isInfixOf [c]) ["1","ABC2","DEF3","GHI4","JKL5","MNO6","PQRS7","TUV8","WXYZ9","*"," 0","#"]

presses :: String -> Int
presses str
    | length str == 0 = 0 :: Int
    | otherwise = keyPresses (toUpper(str !! 0)) + presses (tail str)




---------------
-- Tests
---------------

test = hspec $ do
  describe "presses" $ do
    it "should work for simple examples" $ do
      presses "LOL" `shouldBe` 9
    it "should work for phrases with spaces" $ do
      presses "HOW R U" `shouldBe` 13