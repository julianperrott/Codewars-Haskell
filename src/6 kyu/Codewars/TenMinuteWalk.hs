{- http://www.codewars.com/kata/54da539698b8a2ad76000228 "6 kyu","Take a Ten Minute Walk","54da539698b8a2ad76000228"

You live in the city of Cartesia where all roads are laid out in a perfect grid. You arrived ten minutes too early to an appointment, so you decided to take the opportunity to go for a short walk.
The city provides its citizens with a Walk Generating App on their phones -- everytime you press the button it sends you an array of one-letter strings representing directions to walk (eg. ['n', 's', 'w', 'e']).
You know it takes you one minute to traverse one city block, so create a function that will return true if the walk the app gives you will take you exactly ten minutes (you don't want to be early or late!)
and will, of course, return you to your starting point. Return false otherwise.

Note: you will always receive a valid array containing a random assortment of direction letters ('n', 's', 'e', or 'w' only).
It will never give you an empty array (that's not a walk, that's standing still!).

-}

module Codewars.TenMinuteWalk where
import Control.Monad (when)
import Test.Hspec
import Test.QuickCheck

isValidWalk :: [Char] -> Bool
isValidWalk walk =  length walk == 10 && (isAtStart $ foldr sumWalk (0,0)  $ map move walk)
  where
    sumWalk acc item = (fst acc + fst item,snd acc + snd item)
    isAtStart position = fst position == 0 && snd position ==0
    move 'n' = (1,0)
    move 's' = (-1,0)
    move 'e' = (0,1)
    move 'w' = (0,-1)
    move  _ = (0,0)


test = hspec $ do
  describe "isValidWalk" $ do

    it "should work for some examples 1" $ do  isValidWalk ['n','s','n','s','n','s','n','s','n','s'] ??? "should return True  on valid walk"
    it "should work for some examples 2" $ do isValidWalk ['n','s','n','s','n','s','n','s','n','n'] ??! "should return False on invalid walk"
    it "should work for some examples 3" $ do   isValidWalk ['n','s']    ??! "should return False on too short walk"
    it "should work for some examples 4" $ do   isValidWalk (repeat 'n') ??! "should return False on infinite walk"
    it "should work for some examples 5" $ do   isValidWalk ['n','s','e','w','n','s','e','w','n','s'] ??? "should return True on valid walk"
    it "should reject short walks" $ do
      property $
        forAll (choose (1,9)) $ \n ->
        forAll (listOf1 $ elements "nswe") $ \xs ->
          let walk = take n xs
          in isValidWalk walk ??! "the walk \""++walk++"\" is too short and should be rejected"
    it "should work for semi-random valid walks" $ do
      property $
        forAll (choose (0,3)) $ \n ->
          let k = 5 - n
              w = replicate n 's' ++ replicate n 'n' ++ replicate k 'w' ++ replicate k 'e'
          in isValidWalk w ??? "the walk \""++w++"\" is valid short and should be accepted"

-- | Additional helpers to provide better error messages
--   on boolean functions.
(???), (??!) :: Bool -> String -> Expectation
(??!) p = when p       . expectationFailure
(???) p = when (not p) . expectationFailure

infix 0 ??!, ???
