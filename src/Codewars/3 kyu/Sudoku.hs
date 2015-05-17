{- http://www.codewars.com/kata/5296bc77afba8baa690002d7 "3 kyu","Sudoku Solver","5296bc77afba8baa690002d7"

Write a function that will solve a 9x9 Sudoku puzzle. The function will take one argument consisting of the 2D puzzle array, with the value 0 representing an unknown square.

The Sudokus tested against your function will be "easy" (i.e. determinable; there will be no need to assume and test possibilities on unknowns) and can be solved with a brute-force approach.

For Sudoku rules, see the Wikipedia article.

var puzzle = [
            [5,3,0,0,7,0,0,0,0],
            [6,0,0,1,9,5,0,0,0],
            [0,9,8,0,0,0,0,6,0],
            [8,0,0,0,6,0,0,0,3],
            [4,0,0,8,0,3,0,0,1],
            [7,0,0,0,2,0,0,0,6],
            [0,6,0,0,0,0,2,8,0],
            [0,0,0,4,1,9,0,0,5],
            [0,0,0,0,8,0,0,7,9]];

sudoku(puzzle);
/* Should return
[[5,3,4,6,7,8,9,1,2],
[6,7,2,1,9,5,3,4,8],
[1,9,8,3,4,2,5,6,7],
[8,5,9,7,6,1,4,2,3],
[4,2,6,8,5,3,7,9,1],
[7,1,3,9,2,4,8,5,6],
[9,6,1,5,3,7,2,8,4],
[2,8,7,4,1,9,6,3,5],
[3,4,5,2,8,6,1,7,9]]

-}

module Codewars.Sudoku where
import Test.Hspec

sudoku :: [[Int]] -> [[Int]]
sudoku puzzle
    | (zeroCount puzzle) == 0 = puzzle -- solved
    | zeroCount puzzle == zeroCount newPuzzle = puzzle -- failed to solve
    | otherwise = sudoku newPuzzle
    where newPuzzle = parseSudoku puzzle 0 0
          zeroCount xs = sum $ map length $ map (filter (==0)) xs

parseSudoku :: [[Int]] -> Int -> Int-> [[Int]]
parseSudoku grid row col
    | row == 9 = grid -- stepped throught the whole grid
    | gridRow !! col > 0 = parseSudoku grid (fst next) (snd next) -- skip already solved cell, move to next
    | newCellValue > 0  = newGrid -- return updated grid if a cell has been solved
    | otherwise = parseSudoku grid (fst next) (snd next) -- unable to solve cell so move to next
    where
        next = if col == 8 then ( (row +1) , 0) else (row, (col + 1)) -- step through grid
        newCellValue = solveCell grid row col
        gridRow = grid !! row
        newGridRow = take col gridRow ++ [newCellValue] ++ drop (col + 1) gridRow
        newGrid = take row grid ++ [newGridRow] ++ drop (row + 1) grid

solveCell ::  [[Int]] -> Int -> Int-> Int
solveCell grid row col = if length candidates == 1 then head candidates else 0
    where values = grid !! row ++ (map (!!col) grid) ++ subGridRow 0 ++ subGridRow 1 ++ subGridRow 2  -- row + cols + sub grid
          subGrid n = (3*) $ n `div` 3
          subGridRow n = take 3 $ drop (subGrid col) (grid !! ((subGrid row) + n))
          candidates = filter (`notElem` values) [1..9]



puzzle = [[5,3,0,0,7,0,0,0,0],
          [6,0,0,1,9,5,0,0,0],
          [0,9,8,0,0,0,0,6,0],
          [8,0,0,0,6,0,0,0,3],
          [4,0,0,8,0,3,0,0,1],
          [7,0,0,0,2,0,0,0,6],
          [0,6,0,0,0,0,2,8,0],
          [0,0,0,4,1,9,0,0,5],
          [0,0,0,0,8,0,0,7,9]]

puzzleReplace = [[5,3,0,0,7,0,0,0,0],
          [6,0,0,1,9,5,0,0,0],
          [0,9,8,99,0,0,0,6,0],
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
  it "solveCell puzzle 2 3" $ do solveCell puzzle 2 3  `shouldBe` 0
  it "solveCell puzzle 0 4" $ do solveCell puzzle 0 4  `shouldBe` 4
  it "solveCell puzzle 4 4" $ do solveCell puzzle 4 4  `shouldBe` 5
  it "parseSudoku puzzle 0 4" $ do parseSudoku puzzle 0 0  `shouldBe` [[5,3,0,0,7,0,0,0,0],[6,0,0,1,9,5,0,0,0],[0,9,8,0,0,0,0,6,0],[8,0,0,0,6,0,0,0,3],[4,0,0,8,5,3,0,0,1],[7,0,0,0,2,0,0,0,6],[0,6,0,0,0,0,2,8,0],[0,0,0,4,1,9,0,0,5],[0,0,0,0,8,0,0,7,9]]
  it "parseSudoku2 puzzle 4 4" $ do parseSudoku puzzle 4 4  `shouldBe` [[5,3,0,0,7,0,0,0,0],[6,0,0,1,9,5,0,0,0],[0,9,8,0,0,0,0,6,0],[8,0,0,0,6,0,0,0,3],[4,0,0,8,5,3,0,0,1],[7,0,0,0,2,0,0,0,6],[0,6,0,0,0,0,2,8,0],[0,0,0,4,1,9,0,0,5],[0,0,0,0,8,0,0,7,9]]
