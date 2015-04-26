module Codewars.MoleculeToAtoms where
import           Data.Char
import           Data.Function   (on)
import           Data.List
import           Data.List.Split

import           Test.Hspec
import           Test.HUnit


parseMolecule :: String -> Either String [(String,Int)]
parseMolecule formula
    | extracted == [] = Left "Empty"
    | fst finalItem == -1 = Right parse
    | fst finalItem < -1 = Left (fst (snd finalItem))
    | otherwise = Left "Unknown"
    where
        extracted =  extract formula 0 ' '
        parse = filter (\x -> snd x >0) $ map snd $ extracted
        finalItem = last extracted


extract :: String -> Int -> Char -> [(Int, (String,Int))] -- formula -> index -> endChar -> (index, elements)
extract formula index endChar
    | index < 0 = []
    | length formula <= index = if endChar == ' ' then [(-1,("Finished",0))] else [(-2,("Mismatched parenthesis",0) )] -- unfound end
    | formula !! index == endChar = [(index +1,([endChar],0))] -- end of substring

    | formula !! index == '{' = extractSub formula index  '{' '}' endChar -- start of substrng
    | formula !! index == '[' = extractSub formula index  '[' ']' endChar -- start of substrng
    | formula !! index == '(' = extractSub formula index  '(' ')' endChar -- start of substrng

    | fst element > index  = [element] ++ extract formula (fst element) endChar
    | otherwise = [(-3,("Not a valid molecule",0))]
    where element = extractElement formula index


extractSub :: String -> Int -> Char -> Char -> Char -> [(Int, (String,Int))]
extractSub formula index startChar endChar endChar2 = sub
        where
          subformula = extract formula (index+1) endChar
          subformDigitIndex = (fst (last $ subformula))
          subformulaDigits = startingDigits  $ drop (subformDigitIndex) formula
          postsubdigitIndex = subformDigitIndex + length (subformulaDigits)
          sub  = [(index+1,([startChar],0))] ++ subformulaMultiplied ++ extract formula (postsubdigitIndex) endChar2
          subformulaMultiplied = if length subformulaDigits == 0 then subformula else map (\x -> (fst x,(fst (snd x), (read subformulaDigits) * (snd (snd x)) ))) subformula

--sub  = [(index+1,([startChar],0))] ++ subformulaMultiplied ++ [(postsubdigitIndex,("Mult" ++ [endChar] ++ subformulaDigits ++ [endChar2], -1))] ++ extract formula (postsubdigitIndex) endChar2

extractElement :: String -> Int -> (Int, (String,Int))
extractElement formula index
    | length elementName == 0 = (index,("",0))
    | otherwise = (index + (length elementName) + (length digits), (elementName,count))
    where
        elementName = getElementName (drop index formula)
        digits = startingDigits (drop (index + length elementName) formula)
        count = if length digits == 0 then 1 else read digits


getElementName :: String ->  String -- extracts an element name from the start of the string
getElementName str = longest
        where matches =[""] ++ filter (\x -> take (length x) str == x) elements
              longest = maximumBy (compare `on` length) matches
elements = ["Ac","Al","Am","Sb","Ar","As","At","Ba","Bk","Be","Bi","Bh","B","Br","Cd","Cs","Ca","Cf","C","Ce","Cl","Cr","Co","Cn","Cu","Cm","Ds","Db","Dy","Es","Er","Eu","Fm","Fl","F","Fr","Gd","Ga","Ge","Au","Hf","Hs","He","Ho","H","In","I","Ir","Fe","Kr","La","Lr","Pb","Li","Lv","Lu","Mg","Mn","Mt","Md","Hg","Mo","Nd","Ne","Np","Ni","Nb","N","No","Os","O","Pd","P","Pt","Pu","Po","K","Pr","Pm","Pa","Ra","Rn","Re","Rh","Rg","Rb","Ru","Rf","Sm","Sc","Sg","Se","Si","Ag","Na","Sr","S","Ta","Tc","Te","Tb","Tl","Th","Tm","Sn","Ti","W","Uuo","Uup","Uus","Uut","U","V","Xe","Yb","Y","Zn","Zr"]


startingDigits :: String -> String -- returns a string of digits from the start of a string
startingDigits [] = ""
startingDigits (x:xs)
    | isDigit x = [x] ++ startingDigits xs
    | otherwise = ""



--K4[ON(SO3)2]2

{-
K4 -> ["K",4]
(
        ON ["O",1] ["N",1]
        (
                S O3 ["S",1],["O",3]
        ) 2 -> ["S",2],["O",6]
)2 -> ["O",2] ["N",2] + ["S",4],["O",12]

["K",4] ["O",14] ["N",2] ["S",4]
-}


test = hspec $ do

  describe "extractElement" $ do
      it "H3" $ do extractElement "H3" 0 `shouldBe` (2,("H",3))
      it "H" $ do extractElement "H" 0 `shouldBe` (1,("H",1))
      it "HHe" $ do extractElement "HHe" 0 `shouldBe` (1,("H",1))
      it "He(H" $ do extractElement "He(H" 0 `shouldBe` (2,("He",1))
      it "He2" $ do extractElement "He2" 0 `shouldBe` (3,("He",2))
      it "He(" $ do extractElement "He(" 0 `shouldBe` (2,("He",1))
      it "K4[ON(SO3)2]2 - 0" $ do extractElement "K4[ON(SO3)2]2" 0 `shouldBe` (2,("K",4))
      it "K4[ON(SO3)2]2 - 7" $ do extractElement "K4[ON(SO3)2]2" 7 `shouldBe` (9,("O",3))
  describe "extract" $ do
      it "K4[ON(SO3)2]" $ do extract "K4[ON(SO3)2]" 0 ' ' `shouldBe` [(2,("K",4)),(3,("[",0)),(4,("O",1)),(5,("N",1)),(6,("(",0)),(7,("S",2)),(9,("O",6)),(10,(")",0)),(12,("]",0)),(-1,("Finished",0))]
      it "K4[ON(SO3)2]3" $ do extract "K4[ON(SO3)2]3" 0 ' ' `shouldBe` [(2,("K",4)),(3,("[",0)),(4,("O",3)),(5,("N",3)),(6,("(",0)),(7,("S",6)),(9,("O",18)),(10,(")",0)),(12,("]",0)),(-1,("Finished",0))]
      it "K4[ON(SO3)2]2" $ do extract "K4[ON(SO3)2]2" 0 ' ' `shouldBe` [(2,("K",4)),(3,("[",0)),(4,("O",2)),(5,("N",2)),(6,("(",0)),(7,("S",4)),(9,("O",12)),(10,(")",0)),(12,("]",0)),(-1,("Finished",0))]
      it "K4[ON(SO3)22" $ do extract "K4[ON(SO3)22" 0 ' ' `shouldBe` [(2,("K",4)),(3,("[",0)),(4,("O",1)),(5,("N",1)),(6,("(",0)),(7,("S",22)),(9,("O",66)),(10,(")",0)),(-2,("Mismatched parenthesis",0))]
      it "pie" $ do extract "pie" 0 ' ' `shouldBe` [(-3,("Not a valid molecule",0))]
      it "H" $ do extract "H" 0 ' ' `shouldBe` [(1,("H",1)),(-1,("Finished",0))]
  describe "Molecules" $ do
        assertParse "H" [("H",1)] "hydrogen"
        assertParse "O2" [("O",2)] "oxygen"
        assertParse "H2O" [("H",2),("O",1)] "water"
        assertParse "Mg(OH)2" [("Mg",1),("O",2),("H",2)] "magnesium hydroxide"
        assertParse "K4[ON(SO3)2]2" [("K",4),("O",14),("N",2),("S",4)] "Fremy's salt"


  describe "Errors" $ do
    assertFail "pie" "Not a valid molecule"
    assertFail "Mg(OH" "Mismatched parenthesis"
    assertFail "Mg(OH}2" "Mismatched parenthesis"

assertParse formula expected name = it (name ++ ": " ++ formula) $
  assertEqual "" (Right $ sort expected) (fmap sort $ parseMolecule formula)

assertFail formula name = it (name ++ ": " ++ formula) $
  parseMolecule formula `shouldSatisfy` isLeft

isLeft (Left _) = True
isLeft _ = False



