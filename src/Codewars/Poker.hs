module Codewars.Poker where
import Test.Hspec
import Text.Printf

data Suit = Spades
          | Hearts
          | Diamonds
          | Clubs
          deriving (Eq)

instance Show Suit where
  show Spades = "S"
  show Hearts = "H"
  show Diamonds = "D"
  show Clubs = "C"

data Rank = R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10
          | RJ | RQ | RK | RA
          deriving (Eq,Ord,Enum)

instance Show Rank where
  show r | fromEnum r < 9 = show $ fromEnum r + 2
         | r == RJ        = "J"
         | r == RQ        = "Q"
         | r == RK        = "K"
         | r == RA        = "A"

data Card = Card { rank :: Rank
                 , suit :: Suit
                 } deriving (Eq)

instance Show Card where
  show (Card r s) = show r ++ show s
  showList [] = showString ""
  showList ((Card r s):cs) = showString (show r) .
                             showString (show s) .
                             showString " " .
                             showList cs

newtype Hand = Hand { unHand :: [Card] } deriving (Eq)

instance Show Hand where
  show (Hand cs) = show cs

instance Read Rank where
  readsPrec _ s | "2" <= s && s<= "9" = [(toEnum $ read s - 2, "")]
                | s == "10" = [(R10,"")]
                | s == "J" = [(RJ,"")]
                | s == "Q" = [(RQ,"")]
                | s == "K" = [(RK,"")]
                | s == "A" = [(RA,"")]

instance Read Suit where
  readsPrec _ s | s == "C" = [(Clubs,"")]
                | s == "D" = [(Diamonds,"")]
                | s == "H" = [(Hearts,"")]
                | s == "S" = [(Spades,"")]

instance Read Card where
  readsPrec _ s = [(Card (read $ init s) (read [last s]),"")]

instance Read Hand where
  readsPrec _ s = [(Hand $ map read $ words s,"")]


-- | Write function 
--   betterHand :: Hand -> Hand -> Ordering
betterHand :: Hand -> Hand -> Hand
betterHand = error "Write me"





main = let
  sf1 = read "KD QD JD 10D 9D" :: Hand -- straight flush, highest = K
  sf2 = read "AC KC QC JC 10C" :: Hand -- straight flush, highest = A
  nothing = read "2C 4H 6D 8S 10C" :: Hand -- high card
  sfBig1 = read "8C 4C 5C 6C 7C 9C 10C" :: Hand -- straight flush, 7 cards, highest=R10
  sfBig2 = read "8C 4C 5C JC 7C 9C 10C" :: Hand -- straight flush, 7 cards, highest=R8

  in hspec $ do
  describe "Simple tests" $ do
    it "Straight Flush > High Card" $  betterHand sf1 nothing `shouldBe` sf1
    it "Straight Flush by highest card" $  betterHand sf1 sf2  `shouldBe` sf2
    it "Straight Flush by highest card with 7 cards" $ betterHand sfBig1 sfBig2 `shouldBe` sfBig2
    it "Two equal hands" $ betterHand sfBig1 sfBig1 `shouldBe` sfBig1

