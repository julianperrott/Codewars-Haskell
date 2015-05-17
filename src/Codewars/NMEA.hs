{- http://www.codewars.com/kata/54249c6bf132dcc661000495

GPS receivers use NMEA format to interface with other hardware. Each NMEA sentence starts with a $ character, contains some ASCII data, contains an optional checksum
separated from the data by a * character, and ends with end of line characters CR+LF.

The checksum is a XOR of all data characters not including the starting $. It is represented as two hexadecimal digits.

For example, in the sentence $GPRMC,152226.580,A,37.659,N,54.216,E,0.57,0.17,140924,,*3A the checksum value 0x3A is the XOR sum of all bytes in the
ASCII string GPRMC,152226.580,A,37.659,N,54.216,E,0.57,0.17,140924,,.

Implement the checking function check :: String -> Bool which expects a NMEA sentence (with end of line characters) that contains a checksum, and checks if the checksum is correct.

The data in real-life NMEA sentences consists of comma-separated words and numbers. Random test strings in this kata do not have this structure
but are guaranteed to not contain $ and * in data part.
-}

module Codewars.NMEA where

import Test.Hspec
import Data.Char
import Data.Bits
import Numeric

check :: String -> Bool
check str = elem checksumDigits [textChecksum, '0' : textChecksum]
    where
        (text,checksum) = break ('*'==) $ tail $ fst $ break ('\r'==) str
        textChecksum = map toUpper $ showHex (foldr (xor) 0 (map ord text)) ""
        checksumDigits = map toUpper $ tail checksum

dat :: String -> (String,String)
dat str = break ('*'==) $ tail $ fst $ break ('\r'==) str

ascii :: String -> String
ascii str = fst $ dat str


checkHex :: String -> String
checkHex str = tail $ snd $ dat str

checksum :: String -> Int
checksum str = foldr (xor) 0 (map ord str)

checkInt :: String -> Int
checkInt str
    | length str > 2 = -1
    | notElem (str !! 0) "0123456789ABCDEF" = -1
    | notElem (str !! 1) "0123456789ABCDEF" = -2
    | otherwise = read ("0x" ++ str)

showIt :: Int -> String
showIt n = map toUpper $ showHex n ""

test = hspec $ do
  describe "showIt tests" $ do
      it "Test Correct 1" $
          showIt 255 `shouldBe` "FF"
  describe "checksum tests" $ do
      it "Test Correct 1" $
          checksum "GPRMC,092751.000,A,5321.6802,N,00630.3371,W,0.06,31.66,280511,,,A" `shouldBe` 69
  describe "ascii tests" $ do
      it "Test Correct 1" $
          ascii "$GPAAM,A,A,0.10,N,WPTNME*32\r\n" `shouldBe` "GPAAM,A,A,0.10,N,WPTNME"
  describe "checkHex tests" $ do
      it "Test Correct 1" $
          checkInt "FF" `shouldBe` 255
  describe "checkInt tests" $ do
      it "Test Correct 1" $
          checkHex "$GPAAM,A,A,0.10,N,WPTNME*32\r\n" `shouldBe` "32"
  describe "checkInt tests" $ do
      it "Test Correct 1" $
          checkHex "$GPAAM,A,A,0.10,N,WPTNME*32\r\n" `shouldBe` "32"
  describe "others tests" $ do
        it "Test Correct 2" $
          check "$GPRMC,152226.580,A,37.659,N,54.216,E,0.57,0.17,140924,,*3A\r\n" `shouldBe` True
        it "Test Correct 3" $
          check "$GPGSV,3,2,12,04,31,054,00,06,29,220,29,01,28,302,25,17,26,133,00*74\r\n" `shouldBe` True
        it "Test Incorrect 1" $
          check "$GPRMC,104427.591,A,5920.7009,N,01803.2938,E,0.146345,320.93,141204,,*93\r\n" `shouldBe` False
        it "Test Incorrect 2" $
          check "$GPGSA,A,3,05,24,17,30,02,,,,,,,,5.6,3.3,4.5*T4\r\n" `shouldBe` False
        it "Test Incorrect 3" $
          check "$GPRMC,104427.591,A,5920.7009,N,01803.2938,E,0.146345,320.93,141204,,*0808\r\n" `shouldBe` False
        it "check 0E" $
          check "$IGAR=D/BY3QP;0628M0XXRAFC@02VYQ1,2.DM9FQ,C41S04SU:9UFMHK/V;G-V1LXMK2*0E\r\n"`shouldBe` True
