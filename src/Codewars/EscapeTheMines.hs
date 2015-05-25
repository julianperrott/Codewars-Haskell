module Codewars.EscapeTheMines where
import Test.Hspec

type XY = (Int,Int)
data Move = U | D | R | L deriving (Eq, Show)

solve :: [[Bool]] -> XY -> XY  -> [Move]
solve grid miner exit = explore grid miner exit [] 0

isVisited :: [[Bool]] -> XY -> Bool
isVisited grid (x,y)
  | x < 0 = True
  | y < 0 = True
  | x >= length grid = True
  | y >= length (grid !! 0) = True
  | otherwise = grid !! x !! y == False

move :: XY -> Move -> XY
move (x,y) step
  | step == D = (x, y + 1)
  | step == U = (x, y - 1)
  | step == L = (x - 1, y)
  | step == R = (x + 1, y)
  | otherwise = error "unknown direction!"

setRowVisited :: [Bool] -> Int -> [Bool]
setRowVisited row col = take col row ++ [False] ++ drop (col+1) row

setVisited :: [[Bool]] -> XY -> [[Bool]]
setVisited grid (x,y) = take x grid ++ [(setRowVisited (grid!!x) y)] ++ drop (x+1) grid

explore :: [[Bool]] -> XY -> XY -> [Move] -> Int -> [Move]
explore grid pos exit path depth = headOrEmpty $ filter (\p -> length p > 0) $ map explorePath $ [U,D,R,L]
    where
        headOrEmpty [] = []
        headOrEmpty xs = head xs
        explorePath step
            | depth > 50 = error "explored too far"
            | isVisited grid newPos = []
            | newPos == exit = newPath
            | otherwise = explore (setVisited grid newPos) newPos exit newPath (depth+1)
            where
             newPos = move pos step
             newPath = path ++ [step]

{- 
explorePath :: [[Bool]] -> XY -> XY -> [Move] -> Move -> [Move]
explorePath grid pos exit path step
    | isVisited grid newPos = []
    | newPos == exit = path
    | otherwise = explore (setVisited grid newPos) newPos exit newPath
    where
     newPos = move pos step
     newPath = path ++ [step]

explore :: [[Bool]] -> XY -> XY -> [Move] -> [Move]
explore grid pos exit path = head $ filter (\p -> length p > 0) $ map (\p-> explorePath grid pos exit path p) $ [U,D,R,L]
-}






test = hspec $ do
  describe "A trivial mine (1x1)" $ do
    let mine = [[True]];
    it "Should return an empty list, since we're already at the goal" $ do
      solve mine (0,0) (0,0) `shouldBe` []
 
  describe "A pretty simple mine (2x2)" $ do
    let mine = unmine [" #"
                    ,"  "
                    ]
    it "Should return the only correct move" $ do
      solve mine (0,0) (1,0) `shouldBe` [R]
      
    it "Should return the only moves necessary" $ do
      solve mine (0,0) (1,1) `shouldBe` [R, D]
      
      
  describe "A linear mine(1x4)" $ do
    let mine = unmine [" "
                    ," "
                    ," "
                    ," "
                    ]

    it "Should return a chain of moves to the right" $ do
      solve mine (0,0) (3,0) `shouldBe` [R, R, R]

    it "Should return a chain of moves to the left" $ do
      solve mine (3,0) (0,0) `shouldBe` [L, L, L]

  describe "Should walk around an obstacle (3x3 mine)" $ do
    let mine = unmine ["   "
                    ,"## "
                    ,"   "
                    ]

    it "Should return the right sequence of moves" $ do
      solve mine (0,0) (2,0) `shouldBe` [D, D, R, R, U, U]

  describe "Should be able to change directions multiple times (5x5 mine)" $ do
    let mine = unmine ["  ###"
                    ,"#  ##"
                    ,"##  #"
                    ,"###  "
                    ,"#### "
                    ]

    it "Should return a step sequence of moves" $ do
      solve mine (0,0) (4,4) `shouldBe` [D, R, D, R, D, R, D, R]

  describe "Should avoid dead-ends (5x5 mine)" $ do
    let mine = unmine ["   ##"
                    ,"## # "
                    ,"     "
                    ," # ##"
                    ,"#    "
                    ]

    it "Should return the right moves" $ do
      solve mine (0,0) (4,4) `shouldBe` [D, D, R, R, R, R, D, D]

unmine = map (map (== ' '))