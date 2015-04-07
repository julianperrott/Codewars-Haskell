module Codewars.WindInfo where
import Test.Hspec
import Data.Char

--runway (string: "NN[L/C/R]"). NN is the runway's heading in tens of degrees. A suffix of L, C or R may be present and should be ignored. NN is between 01 and 36.
--wind_direction (int). Direction wind is blowing from in degrees. Between 0 and 359.
-- wind_speed (int). Wind speed in knots

windInfo :: [Char] -> Int -> Int -> [Char]
windInfo runway windDir windSpeed = headDesc ++ " " ++ (show $ abs headWind) ++ " knots. Crosswind " ++ (show $ abs crossWind) ++ " knots from your " ++ fromDesc ++ "."
    where runwayDegrees = ( (100*) $ digitToInt $ runway !! 0) + ((10*) $ digitToInt $ runway !! 1)
          angle = (/180) $ (*pi) $ fromIntegral $ windDir - runwayDegrees
          headWind =  round $ (fromIntegral windSpeed) * (cos angle)
          crossWind = round $ (fromIntegral windSpeed) * (sin angle)
          headDesc = if headWind >= 0 then "Headwind" else "Tailwind"
          fromDesc = if crossWind >= 0 then "right" else "left"

---------------
-- Tests
---------------



test = hspec $ do
  describe "Testing WindInfo"$ do
    it "for 170" $ do
      (windInfo "18" 170 15) `shouldBe` "Headwind 15 knots. Crosswind 3 knots from your left."
    it "for 210" $ do
      (windInfo "18" 210 14) `shouldBe` "Headwind 12 knots. Crosswind 7 knots from your right."
    it "for 160" $ do
      (windInfo "22L" 160 14) `shouldBe` "Headwind 7 knots. Crosswind 12 knots from your left."
    it "for 70" $ do
      (windInfo "18" 70 15)  `shouldBe` "Tailwind 5 knots. Crosswind 14 knots from your left."
