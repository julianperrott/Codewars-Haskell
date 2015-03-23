module SequenceSum where

sumIt :: Int -> Int
sumIt n = sum[0..n]

sumOfN :: Int -> [Int]
sumOfN n
    | n < 0 = map (*(-1)) (sumOfN (abs(n)))
    | otherwise = map sumIt [0..n]