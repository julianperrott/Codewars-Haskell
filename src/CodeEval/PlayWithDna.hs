module CodeEval.PlayWithDna where

{- https://www.codeeval.com/open_challenges/126/

PLAY WITH DNA
CHALLENGE DESCRIPTION:

This challenge is related to bioinformatics. To help in a DNA research, you have to write an algorithm that finds all the occurrences of a DNA segment in a given DNA string. But, it would be too easy for you. So, write an algorithm with the maximum N number of allowed mismatches. By mismatch we mean the minimum of the total number of substitutions, deletions, and insertions that must be performed on a DNA slice to completely match a given segment. You need to compare the DNA slices with the same length as a given pattern (for example, the segments 'AGTTATC' -> 'AGTATGC' have only 2 mismatches).

For the DNA string 'CGCCCGAATCCAG' and the segment 'CCC', the first match with the maximum 1 allowed mismatch is 'CGC', the second one is 'GCC', the third one is 'CCC', and so on.

CCC -> CGC # One mismatch
CCC -> GCC # One mismatch
CCC -> CCC # 0 mismatch
For the given segment 'CCC', the DNA string 'CGCCCGAATCCAG', and the maximum allowed mismatch '1', the list of the matches is 'CGC GCC CCC CCG TCC CCA'.

INPUT SAMPLE:

Your program should accept a path to a filename as its first argument. Each line in the file contains a segment of DNA, the maximum number of allowed mismatches N, and a DNA string separated by a single space.

CCC 1 CGCCCGAATCCAG
GCGAG 2 CCACGGCCTATGTATTTGCAAGGATCTGGGCCAGCTAAATCAGCACCCCTGGAACGGCAAGGTT
CATTTTGTTGCGCGCATAG
CGGCGCC 1 ACCCCCGCAGCCATATGTCCCCAGCTATTTAATGAGGGCCCCGAACACGGGGAGTCTTACAC
GATCTGCCCTGGAATCGC
OUTPUT SAMPLE:

Print out all the occurrences of a segment S in a DNA string in the order from the best match (separated by a single space) taking into account the number of allowed mismatches. In case of several segments with the equal number of matches, print them in alphabetical order. If there is no such a case, print out 'No match'.

CCC CCA CCG CGC GCC TCC
GCAAG GCAAG GCCAG GCGCG GCGCA GCTAA
No match
CONSTRAINTS:

The length of a DNA string is in a range from 100 to 300.
N is in a range from 0 to 40.
The length of a segment S is in a range from 3 to 50.
-}

import Test.Hspec

import Data.List
import Data.List.Split

tpsort f a b  | f a == f b = EQ
         | f a < f b = LT
         | otherwise = GT

playWithDna::String->String
playWithDna line
  | length occurrences == 0 = "No match"
  | otherwise = intercalate " " occurrences
  where
    candidates = map (\x -> take (length seg) $ drop x dna) [0..(length dna)-(length seg)]
    withDifferences = map (\c -> (c, countDifferences seg c)) candidates
    occurrences = map fst $ sortBy (tpsort snd) $ sortBy (tpsort fst) $ filter (\x -> (snd x)<=n ) withDifferences
    args = splitOn " " line
    seg = args!!0
    n = read (args!!1)::Int
    dna = args!!2

countDifferences:: String->String->Int
countDifferences s1 s2
  | s1==s2 = 0
  | otherwise = foldr (+) 0 $ map isSame [0..(length s1)-1]
  where
    isSame x
      | s1!!x==s2!!x = 0
      | otherwise = 1


test = hspec $ do
  describe "playWithDna" $ do
         it "1" $ do playWithDna "CCC 1 CGCCCGAATCCAG" `shouldBe` "CCC CCA CCG CGC GCC TCC"
         it "2" $ do playWithDna "GCGAG 2 CCACGGCCTATGTATTTGCAAGGATCTGGGCCAGCTAAATCAGCACCCCTGGAACGGCAAGGTTCATTTTGTTGCGCGCATAG" `shouldBe` "GCAAG GCAAG GCCAG GCGCG GCGCA GCTAA"
         it "3" $ do playWithDna "CGGCGCC 1 ACCCCCGCAGCCATATGTCCCCAGCTATTTAATGAGGGCCCCGAACACGGGGAGTCTTACACGATCTGCCCTGGAATCGC" `shouldBe` "No match"