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
