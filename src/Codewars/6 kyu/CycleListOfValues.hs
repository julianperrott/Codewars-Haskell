{- http://www.codewars.com/kata/5456812629ccbf311b000078 "6 kyu","Cycle a list of values","5456812629ccbf311b000078"

Prologue

You're part of a team porting MS Paint into the browser and currently working on a new UI component that allows user to control the canvas zoom level.

According to the wireframes delivered to you in PowerPoint format the user should be able to cycle through specified zoom levels by clicking a button in the UI repeatedly. The reverse direction should work with shift key held.

A new function is needed to support this behavior, so you alt-tab to Visual Studio and get to work.

Instructions

Implement a function which when given the arguments

Direction to which to cycle the current value
List of values
Current value
returns the value next to current value in the specified direction.

The function should pick the next value from the other side of the list in case there are no values in the given direction.

Examples

cycle(1, [1,2,3], 1)   // => 2
// Given the direction 1, returns the value next to 1 on the right

cycle(-1, [1,2,3], 1)  // => 3
// Given the direction -1 and value 1, wraps around list returning the last element

cycle(1, [1,2,3], 0)   // => null
// 0 does not exist in the list, returns null

cycle(1, [1,2,2,3], 2) // => 2
// Corner case: multiple instances of given value, picks next relative to first occurrence
-}

module Codewars.CycleListOfValues where
import           Data.List
import           Data.Maybe
import           Test.Hspec
import           Test.QuickCheck

data Direction = L | R deriving (Show)

cycleList :: (Eq a) => Direction -> [a] -> a -> Maybe a
cycleList dir valueList value
    | length valueIndicies == 0 = Nothing -- value not found
    | show dir == "L" = leftValue
    | otherwise = rightValue
    where
          valueIndicies = elemIndices value valueList
          valueIndex = head valueIndicies
          getValue =  \x -> Just $ valueList !! x
          lastIndex = (length valueList) -1
          leftValue = if valueIndex == 0 then getValue lastIndex else getValue $ valueIndex-1
          rightValue = if valueIndex == lastIndex then getValue 0 else getValue $ valueIndex+1


---------------
-- Tests
---------------

test = hspec $ do
  it "gets the next value to the left" $ do
    cycleList L [1,2,3] 3 `shouldBe` Just 2
    cycleList L [1,2,3] 2 `shouldBe` Just 1
    cycleList L [1,2,3] 1 `shouldBe` Just 3
  it "gets the next value to the right" $ do
    cycleList R [1,2,3] 3 `shouldBe` Just 1
    cycleList R [1,2,3] 2 `shouldBe` Just 3
    cycleList R [1,2,3] 1 `shouldBe` Just 2
  it "returns Nothing if value is not in the list" $ do
    cycleList L [1,2,3] 4 `shouldBe` Nothing

{-
http://www.codewars.com/kata/5456812629ccbf311b000078/train/haskell
[Prologue]

You're part of a team porting MS Paint into the browser and currently working on a new UI component that allows user to control the canvas zoom level.

According to the wireframes delivered to you in PowerPoint format the user should be able to cycle through specified zoom levels by clicking a button in the UI repeatedly. The reverse direction should work with shift key held.

A new function is needed to support this behavior, so you alt-tab to Visual Studio and get to work.

[Instructions]

Implement a function which when given the arguments

Direction to which to cycle the current value
List of values
Current value
returns the value next to current value in the specified direction.

The function should pick the next value from the other side of the list in case there are no values in the given direction.

[Examples]

cycleList R [1,2,3] 1  -- => Just 2
cycleList L [1,2,3] 1  -- => Just 3
cycleList R [1,2,3] 0  -- => Nothing
cycleList L ["foo", "bar", "xyz"] "bar"  -- => Just "foo"

-}
