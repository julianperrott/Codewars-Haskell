module Codewars.Sudoku where
import Test.Hspec


sudoku :: [[Int]] -> [[Int]]
sudoku puzzle 
    | (0==) $ unsolvedCount puzzle = puzzle -- solved
    | unsolvedCount puzzle == unsolvedCount newPuzzle = puzzle -- failed to solve
    | otherwise = sudoku newPuzzle
    where newPuzzle = solve puzzle

unsolvedCount :: [[Int]] -> Int
unsolvedCount puzzle = sum $ map length $ map (filter (==0)) puzzle

solve :: [[Int]] -> [[Int]]
solve puzzle = [[1]]
    where newPuzzle = 


solveCell :: (Int,Int)-> [[Int]] -> (Int,Int,Int) -- row col array = value
solveCell rowcol puzzle = (1,2,3)


puzzle = [[5,3,0,0,7,0,0,0,0],
          [6,0,0,1,9,5,0,0,0],
          [0,9,8,0,0,0,0,6,0],
          [8,0,0,0,6,0,0,0,3],
          [4,0,0,8,0,3,0,0,1],
          [7,0,0,0,2,0,0,0,6],
          [0,6,0,0,0,0,2,8,0],
          [0,0,0,4,1,9,0,0,5],
          [0,0,0,0,8,0,0,7,9]]

--for each cell create a row of values -- remove 

solution = [[5,3,4,6,7,8,9,1,2],
            [6,7,2,1,9,5,3,4,8],
            [1,9,8,3,4,2,5,6,7],
            [8,5,9,7,6,1,4,2,3],
            [4,2,6,8,5,3,7,9,1],
            [7,1,3,9,2,4,8,5,6],
            [9,6,1,5,3,7,2,8,4],
            [2,8,7,4,1,9,6,3,5],
            [3,4,5,2,8,6,1,7,9]]

test = hspec $ describe "Sudoku" $ do
  it "Puzzle 1" $ do sudoku puzzle `shouldBe` solution
  it "isSolved puzzle" $ do unsolvedCount puzzle `shouldBe` 51
  it "isSolved solution" $ do unsolvedCount solution `shouldBe` 0