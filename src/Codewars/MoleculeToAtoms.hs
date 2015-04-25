module Codewars.MoleculeToAtoms where
import Data.List
import Data.List.Split
import Data.Char
import Data.Function (on)

import Test.Hspec
import Test.HUnit


parseMolecule :: String -> Either String [(String,Int)]
parseMolecule formula
    | snd finalItem == 0 = Left (fst finalItem)
    | otherwise = Right parse
    where parse =[("doh",0)]
          finalItem = parse !! length parse

getElement :: String ->  [(String,Int)]
getElement formula
    | length formula == 0 = [("hello",0)]
    | snd element > 0 = [element] ++ (getElement (drop n formula)) -- remove elements found
    | otherwise = [("hello",0)]
    where
        element = extractElement formula
        n = 1




extract :: String -> Int -> Char -> (Int, [(String,Int)]) -- formula -> index -> endChar -> (index, elements)
extract formula index endChar
    | length formula < index = if endChar == ' ' then (0,[]) else error "doh" -- unfound end
    | formula !! index == endChar = (index +1,[]) -- end of substring
    | formula !! index == '{' = extract formula (index+1) '}'
    | formula !! index == '[' = extract formula (index+1) ']'
    | formula !! index == '(' = extract formula (index+1) ')'
    | snd element > 0  = pah !
    
    where substring = drop index formula
          element = extractElement substring

--if this an element = ++ element and number and loop
--if this a start block recurse then continue
--if this is an unexpected close then error
--if this is an expected close the return
--otherwise WTF is it !


getElementName :: String -> String -- extracts an element name from the start of the string
getElementName str = longest
        where matches =[""] ++ filter (\x -> take (length x) str == x) elements
              longest = maximumBy (compare `on` length) matches

elements = ["Ac","Al","Am","Sb","Ar","As","At","Ba","Bk","Be","Bi","Bh","B","Br","Cd","Cs","Ca","Cf","C","Ce","Cl","Cr","Co","Cn","Cu","Cm","Ds","Db","Dy","Es","Er","Eu","Fm","Fl","F","Fr","Gd","Ga","Ge","Au","Hf","Hs","He","Ho","H","In","I","Ir","Fe","Kr","La","Lr","Pb","Li","Lv","Lu","Mg","Mn","Mt","Md","Hg","Mo","Nd","Ne","Np","Ni","Nb","N","No","Os","O","Pd","P","Pt","Pu","Po","K","Pr","Pm","Pa","Ra","Rn","Re","Rh","Rg","Rb","Ru","Rf","Sm","Sc","Sg","Se","Si","Ag","Na","Sr","S","Ta","Tc","Te","Tb","Tl","Th","Tm","Sn","Ti","W","Uuo","Uup","Uus","Uut","U","V","Xe","Yb","Y","Zn","Zr"]




extractElement :: String -> (String,Int)
extractElement str = (elementName, count)
    where
        elementName = getElementName str
        count = if length elementName ==0 then 0 else getElementCount (drop (length elementName) str)


getElementCount :: String -> Int -- returns the number at the start of a string or 1 if there is no number
getElementCount str = if length digits == 0 then 1 else read digits
    where digits = startingDigits str

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

  describe "findEndOfNode" $ do
      it "ON(SO3)ABC]DEF" $ do findEndOfNode  '[' ']' "ON(SO3)ABC]DEF" `shouldBe` "ON(SO3)ABC"
      it "ON[SO3]ABC]DEF" $ do findEndOfNode  '[' ']' "ON[SO3]ABC]DEF" `shouldBe` "ON[SO3]ABC"
      it "ON[SO[s][ds](3)]2]s[2[232]]" $ do findEndOfNode  '[' ']' "ON[SO[s][ds](3)]2]s[2[232]]" `shouldBe` "ON[SO[s][ds](3)]2"

{-
  describe "splitOnFirstBracket" $ do
      it "H()1" $ do splitOnFirstBracket "H()1" `shouldBe` "H"
      it "H[1" $ do splitOnFirstBracket "H[1" `shouldBe` "H"
      it "H{1" $ do splitOnFirstBracket "H{1" `shouldBe` "H"
      it "HD2(s1" $ do splitOnFirstBracket "HD2(s1" `shouldBe` "HD2"

  describe "getElementName" $ do
      it "H1" $ do getElementName "H1" `shouldBe` "H"
      it "He1" $ do getElementName "He1" `shouldBe` "He"
      it "He(" $ do getElementName "He(" `shouldBe` "He"
      it "(He" $ do getElementName "(He" `shouldBe` ""

  describe "extractElement" $ do
      it "H3" $ do extractElement "H3" `shouldBe` ("H",3)
      it "H" $ do extractElement "H" `shouldBe` ("H",1)
      it "HHe" $ do extractElement "HHe" `shouldBe` ("H",1)
      it "He(H" $ do extractElement "He(H" `shouldBe` ("He",1)
      it "He2" $ do extractElement "He2" `shouldBe`  ("He",2)
      it "He(" $ do extractElement "He(" `shouldBe` ("He",1)

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

-}


--- Not used

splitOnFirstBracket :: String -> String
splitOnFirstBracket str= head $ splitOneOf "([{" str

findEndOfNode ::  Char -> Char -> String -> String
findEndOfNode openChar closeChar str
    | length stringBeforeCloseChar == 0 = "" -- close not found
    | openChar `elem` stringBeforeCloseChar = subNode ++ [closeChar] ++ nodeAfter subNode -- does end bracket fragment contains a start bracket
    | otherwise = stringBeforeCloseChar 
    where stringBeforeCloseChar = (splitOn [closeChar] str) !! 0
          stringBeforeOpentChar = (splitOn [openChar] str) !! 0
          subNode = stringBeforeOpentChar ++ [openChar] ++ nodeAfter stringBeforeOpentChar
          nodeAfter z= findEndOfNode openChar closeChar (drop (1 + length z) str)


data Tree a = Empty | Node a [Tree a] deriving (Show)

--extractTree :: String -> Tree
--extractTree str =

