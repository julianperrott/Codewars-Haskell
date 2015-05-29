import Codewars.EscapeTheMines

--main=test

import Data.List

pad:: String -> String
pad s = replicate (4- (length s)) ' ' ++s

--main = map putStrLn $ map (\x -> intercalate " " $ map (\y -> pad $ show (y*x)) [1..12])[1..12] 


