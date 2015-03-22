module Circle where

circleArea :: Double -> Maybe Double
circleArea x
   | x <= 0 = Nothing
   | otherwise = Just (pi * x * x) 