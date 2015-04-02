module Codewars.VowelCount where
import Test.Hspec
import Test.QuickCheck
import Data.List

getCount :: String -> Int
getCount = length . filter (\x -> x=='a' || x=='e' || x=='i' || x=='o' || x=='u')


--length . flip intersect "aeiou" 
--length $ str `intersect` "aeiou"

--An anonymous function is a function without a name. It is a Lambda abstraction and might look like this: 
-- \x -> x + 1. 
--(That backslash is Haskell's way of expressing a λ and is supposed to look like a Lambda.)
--Prompt> (\x y -> x + y) 3 5
--8 :: Integer

-- The intersect function takes the list intersection of two lists. For example, 
-- > [1,2,3,4] `intersect` [2,4,6,8] == [2,4] 
-- If the first list contains duplicates, so will the result. 
-- > [1,2,2,3,4] `intersect` [6,4,4,2] == [2,2,4] It is a special case of intersectBy, 


-- flip :: (a -> b -> c) -> b -> a -> c
-- base Prelude, base Data.Function
-- flip f takes its (first) two arguments in the reverse order of f.

---------------
-- Tests
---------------

test = hspec $ do
  describe "getCount" $ do
    it "should work for some examples" $ do
      getCount "ape"         `shouldBe` 2      
      getCount "banana"      `shouldBe` 3
      getCount "batman"      `shouldBe` 2
      getCount "abracadabra" `shouldBe` 5

    it "should work for vowel-only strings" $ do
      property $ \n -> n >= 0 ==>
        getCount (take n . cycle $ "aeiou") `shouldBe` n
    it "should work for consonant strings" $ do
      property $ \n -> n >= 0 ==>
        getCount (take n . cycle $ "bcdfghjklmnpqrstvwxyz") `shouldBe` 0