{- http://www.codewars.com/kata/526a569ca578d7e6e300034e "4 kyu","Base Conversion","526a569ca578d7e6e300034e"

In this kata you have to implement a base converter, which converts between arbitrary bases / alphabets. Here are some pre-defined alphabets:

var Alphabet = {
  BINARY:        '01',
  OCTAL:         '01234567',
  DECIMAL:       '0123456789',
  HEXA_DECIMAL:  '0123456789abcdef',
  ALPHA_LOWER:   'abcdefghijklmnopqrstuvwxyz',
  ALPHA_UPPER:   'ABCDEFGHIJKLMNOPQRSTUVWXYZ',
  ALPHA:         'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ',
  ALPHA_NUMERIC: '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'
};
The function convert() should take an input (string), the source alphabet (string) and the target alphabet (string). You can assume that the input value always consists of characters from the source alphabet. You don't need to validate it.

Examples:

// convert between numeral systems
convert("15", Alphabet.DECIMAL, Alphabet.BINARY); // should return "1111"
convert("15", Alphabet.DECIMAL, Alphabet.OCTAL); // should return "17"
convert("1010", Alphabet.BINARY, Alphabet.DECIMAL); // should return "10"
convert("1010", Alphabet.BINARY, Alphabet.HEXA_DECIMAL); // should return "a"

// other bases
convert("0", Alphabet.DECIMAL, Alphabet.ALPHA); // should return "a"
convert("27", Alphabet.DECIMAL, Alphabet.ALPHA_LOWER); // should return "bb"
convert("hello", Alphabet.ALPHA_LOWER, Alphabet.HEXA_DECIMAL); // should return "320048"
convert("SAME", Alphabet.ALPHA_UPPER, Alphabet.ALPHA_UPPER); // should return "SAME"
Additional Notes:

The maximum input value can always be encoded in a number without loss of precision in JavaScript. In Haskell, intermediate results will probably be to large for Int.
The function must work for any arbitrary alphabets, not only the pre-defined ones
You don't have to consider negative numbers

-}

module Codewars.BaseConversion where
import Data.List
import Data.Maybe
import Test.Hspec
import Test.QuickCheck

newtype Alphabet = Alphabet { getDigits :: [Char] } deriving (Show)

toDec :: Alphabet -> String -> Integer
toDec (Alphabet fromAl) str = sum vals
      where
       fromLength = toInteger $ length fromAl
       charIndex = reverse $ map (\x -> elemIndex x fromAl) str
       vals = map (\t -> (fromLength ^ (toInteger $ snd t)) * (toInteger $ fromJust $ fst t) ) $ zip charIndex [0..]

decToBaseIndex :: Integer -> Integer -> [Integer]
decToBaseIndex _ 0 = []
decToBaseIndex len i =  [i `mod` len] ++ (decToBaseIndex len ((i-(i `mod` len)) `div` len ))

convert :: Alphabet -> Alphabet -> String -> String
convert (Alphabet fromAl) (Alphabet toAl) str = if length converted >0 then converted else [toAl !! 0]
    where converted = map (\i -> toAl !! (fromIntegral i)) $ reverse $ decToBaseIndex (toInteger $ length toAl) $ toDec (Alphabet fromAl) str


--     (fun s-> if s.Length=0 then (Array.get b 0).ToString() else s)



bin, oct, dec, hex, alphaLower, alphaUpper, alpha, alphaNumeric :: Alphabet
bin = Alphabet $ "01"
oct = Alphabet $ ['0'..'7']
dec = Alphabet $ ['0'..'9']
hex = Alphabet $ ['0'..'9'] ++ ['a'..'f']
alphaLower    = Alphabet $ ['a'..'z']
alphaUpper    = Alphabet $ ['A'..'Z']
alpha         = Alphabet $ ['a'..'z'] ++ ['A'..'Z']
alphaNumeric  = Alphabet $ ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z']

allAlphabets  = [bin, oct, dec, hex, alphaLower, alphaUpper, alpha, alphaNumeric]

test = hspec $ do
  describe "convert" $ do
    it "revert alphaLower hex" $ do (convert hex alphaLower $ convert alphaLower hex "hello") `shouldBe` "hello"
    it "revert alphaNumeric alpha" $ do (convert alphaNumeric alpha $ convert alpha alphaNumeric "EaCfEvxNcbScvR") `shouldBe` "EaCfEvxNcbScvR"

    it "should work on some simple examples" $ do
      convert dec bin "15"   `shouldBe` "1111"
      convert dec oct "15"   `shouldBe` "17"
      convert bin dec "1010" `shouldBe` "10"
      convert bin hex "1010" `shouldBe` "a"

      convert dec alpha      "0"     `shouldBe` "a"
      convert dec alphaLower "27"    `shouldBe` "bb"
      convert alphaLower hex "hello" `shouldBe` "320048"
