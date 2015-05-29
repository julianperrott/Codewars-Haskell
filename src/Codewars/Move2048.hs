module Codewars.Move2048 where

import System.Environment
import System.Exit


main = readFile "C:/Users/Julian/Desktop/EscapeTheMines.hs"  >>= putStr . process

parse [] = usage  >> exit
parse [f] = readFile f

firstWord :: String ->String
firstWord s
    | x == [] = "doh"
    | otherwise = head x
      where x = words s


process s = unlines $ map firstWord $ lines s

usage   = putStrLn "Usage: foo <file>"
exit    = exitWith ExitSuccess




move:: [Int] -> [Int]
move xs = replicate (length xs - length merged) 0 ++ merged
  where
    merged = foldr merge [] $ filter (>0) xs
    merge x [] = [x]
    merge x xs
      | head xs == x = [x+x] ++ tail xs
      | otherwise = [x] ++ xs




move [2,0,2,0]




let line = "LEFT; 8; 2 2 2 2 2 0 0 0|4 0 2 2 2 0 2 8|16 4 0 2 0 256 4 2|2 8 2 8 8 0 0 4|0 0 0 2 0 0 8 2|0 0 2 0 0 2 4 0|0 4 4 2 2 8 2 0|0 2 2 0 2 0 0 0"

import Data.List.Split
let params = splitOn [';'] line
let direction = head params
let grid = map (map (\x ->read x::Int)) $ map (splitOn [' ']) $ splitOn ['|'] $ tail $ params!!2


grid

moveLeft :: [[Int]] -> [[Int]]
moveLeft = map (reverse . move . reverse)

moveRight :: [[Int]] -> [[Int]]
moveRight = map move

rotateAntiClockWise:: [[Int]] -> [[Int]]
rotateAntiClockWise xs =  map (\col-> map (\row -> row!!col) xs) $ reverse [0..(length (xs!!0)-1)]

rotateClockWise:: [[Int]] -> [[Int]]
rotateClockWise xs =  map (\col-> map (\row -> row!!col) (reverse xs)) $ [0..(length (xs!!0)-1)]

moveUp :: [[Int]] -> [[Int]]
moveUp = rotateAntiClockWise . (map move) . rotateClockWise

moveDown :: [[Int]] -> [[Int]]
moveDown = rotateClockWise . (map move) . rotateAntiClockWise

rotate grid
