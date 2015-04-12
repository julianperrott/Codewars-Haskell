module Codewars.ProcessTree where
import Test.Hspec

type PID = Int
data Process = Process PID [Process] deriving (Show)


makeTree :: [(PID, PID)] -> Process
makeTree ar = extractTree ar 1

extractTree :: [(PID, PID)] -> PID -> Process
extractTree ar pid = Process pid [ extractTree ar (fst y) | y <- ar, snd y == pid ]



test = hspec $
  describe "makeTree" $ do
    it "should work for the example" $ 
      show (makeTree [(1, -1), (219, 214), (214, 1), (124,1) ]) `shouldBe` "Process 1 [Process 124 [], Process 214 [Process 219 []]]"