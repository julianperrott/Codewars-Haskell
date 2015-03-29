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