{- http://www.codewars.com/kata/52774a314c2333f0a7000688

Write a function called validParentheses that takes a string of parentheses, and determines if the order of the parentheses is valid. validParentheses should return true if the string is valid, and false if it's invalid.

Examples:
validParentheses( "()" ) => returns true
validParentheses( ")(()))" ) => returns false
validParentheses( "(" ) => returns false
validParentheses( "(())((()())())" ) => returns true

All input strings will be nonempty, and will only consist of open parentheses '(' and/or closed parentheses ')'
-}

module Codewars.ValidParentheses where
import Data.List

validParentheses :: String -> Bool
validParentheses str = hasNoNegativeValues && lastValueIsZero
        where sumList = snd $ mapAccumL (\x y -> (x+y,x+y)) 0 [if x=='(' then 1 else -1  |x <- str]
              hasNoNegativeValues = length (filter (<0) sumList) == 0
              lastValueIsZero = (last sumList) == 0
