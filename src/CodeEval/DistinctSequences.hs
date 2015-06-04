module CodeEval.DistinctSequences where
import Test.Hspec

sequenceCount :: String -> String -> [[Int]]
sequenceCount line sequence = seq 0 []
  where 
    seq :: Int -> [Int] -> [[Int]]
    seq linePos path = found ++ notFound
      where
        newPath = path++[linePos]
        charsToFind :: Int
        charsToFind = length sequence - length path
        foundAtPosition :: [[Int]]
        foundAtPosition
            | charsToFind == 1 = [newPath]
            | otherwise = seq (linePos+1) newPath
        found :: [[Int]]
        found
            | line!!linePos == sequence!!(length path) = foundAtPosition
            | otherwise = []
        notFound :: [[Int]]
        notFound 
            | charsToFind < length line - linePos = seq (linePos+1) path
            | otherwise = []

test = hspec $ do
  describe "various" $ do
    it "bag" $ do (length $ sequenceCount "babgbag" "bag") `shouldBe` 5
    it "rabbit" $ do (length $ sequenceCount "rabbbit" "rabbit") `shouldBe` 3
