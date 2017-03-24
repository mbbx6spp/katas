{-# LANGUAGE NoImplicitPrelude #-}

-- | Types to support the PokerHand kata
module Katas.PokerHand.Types where

import           GHC.Base
import           GHC.Classes (Eq (..), Ord (..))
import           GHC.Enum    (Bounded (..), Enum (..))
import           GHC.Show    (Show (..))

-- * CardRank

-- @
--                       _                 _
--      ___ __ _ _ __ __| |_ __ __ _ _ __ | | __
--     / __/ _` | '__/ _` | '__/ _` | '_ \| |/ /
--    | (_| (_| | | | (_| | | | (_| | | | |   <
--     \___\__,_|_|  \__,_|_|  \__,_|_| |_|_|\_\
--
-- @

-- | Repesent a card's rank as a coproduct/sum type
data CardRank =
    NumCard Int
  | Jack
  | Queen
  | King
  | Ace
  deriving (Eq)

-- | Define min and max bounds for CardRank
--
-- Allows us to define ranges over CardRank values.
--
-- So we can generate the list of values of all royalty:
--
-- >>> [Jack .. King]
-- [J,Q,K]
--
-- Or the numbered cards:
--
-- >>> [(minBound :: CardRank) .. (NumCard 10)]
-- [2,3,4,5,6,7,8,9,10]
--
-- Or all of the cards of a suit, say Diamonds, via a list comprehension:
--
-- >>> [ Card r Diamonds | r <- [(minBound :: CardRank) .. (maxBound :: CardRank)] ]
-- [2D,3D,4D,5D,6D,7D,8D,9D,10D,JD,QD,KD,AD]
--
instance Bounded CardRank where
  minBound = NumCard 2
  maxBound = Ace

-- | Define rank as an enumeration
--
-- Examples of how CardRank values should be ordered:
--
-- >>> succ Ace
-- *** Exception: Enum.CardRank.succ: no successor to Ace
--
-- >>> pred Jack
-- 10
--
-- >>> toEnum 11 :: CardRank
-- J
--
-- >>> fromEnum Ace
-- 14
--
-- >>> succ (NumCard 11)
-- *** Exception: Enum.CardRank.succ: number card out of bounds
instance Enum CardRank where
  succ (NumCard 2)  = NumCard 3
  succ (NumCard 3)  = NumCard 4
  succ (NumCard 4)  = NumCard 5
  succ (NumCard 5)  = NumCard 6
  succ (NumCard 6)  = NumCard 7
  succ (NumCard 7)  = NumCard 8
  succ (NumCard 8)  = NumCard 9
  succ (NumCard 9)  = NumCard 10
  succ (NumCard 10) = Jack
  succ Jack         = Queen
  succ Queen        = King
  succ King         = Ace
  succ Ace          = errorWithoutStackTrace "Enum.CardRank.succ: no successor to Ace"
  succ (NumCard _)  = errorWithoutStackTrace "Enum.CardRank.succ: number card out of bounds"

  pred Ace          = King
  pred King         = Queen
  pred Queen        = Jack
  pred Jack         = NumCard 10
  pred (NumCard 10) = NumCard 9
  pred (NumCard 9)  = NumCard 8
  pred (NumCard 8)  = NumCard 7
  pred (NumCard 7)  = NumCard 6
  pred (NumCard 6)  = NumCard 5
  pred (NumCard 5)  = NumCard 4
  pred (NumCard 4)  = NumCard 3
  pred (NumCard 3)  = NumCard 2
  pred (NumCard 2)  = errorWithoutStackTrace "Enum.CardRank.succ: no predecessor to 2"
  pred (NumCard _)  = errorWithoutStackTrace "Enum.CardRank.succ: number card out of bounds"

  toEnum n | n >=2 && n <= 10 = NumCard n
           | n == 11        = Jack
           | n == 12        = Queen
           | n == 13        = King
           | n == 14        = Ace
           | otherwise     = errorWithoutStackTrace "Enum.CardRank.toEnum: not valid index"

  fromEnum Ace         = 14
  fromEnum King        = 13
  fromEnum Queen       = 12
  fromEnum Jack        = 11
  fromEnum (NumCard n) = n

-- | Define string representation of the rank based on kata rules
--
-- >>> show Queen
-- "Q"
--
-- >>> show (NumCard 2)
-- "2"
--
-- >>> show (NumCard 21)
-- "-"
instance Show CardRank where
  show Jack        = "J"
  show Queen       = "Q"
  show King        = "K"
  show Ace         = "A"
  show (NumCard n) | n >= 2 && n <= 10 = show n
  show (NumCard _) | otherwise      = "-"

-- | Using rules in the Kata order ranks
--
-- >>> Ace > King
-- True
--
-- >>> Ace == King
-- False
--
-- >>> NumCard 2 <= Jack
-- True
--
-- >>> NumCard 2 > NumCard 1
-- True
-- FIXME: Is the above what we really want?????
instance Ord CardRank where
  compare Ace         Ace         = EQ
  compare Ace         _           = GT
  compare _           Ace         = LT
  compare King        King        = EQ
  compare King        _           = GT
  compare _           King        = LT
  compare Queen       Queen       = EQ
  compare Queen       _           = GT
  compare _           Queen       = LT
  compare Jack        Jack        = EQ
  compare Jack        _           = GT
  compare _           Jack        = LT
  compare (NumCard m) (NumCard n) = compare m n -- defer comparison to Int

-- * Suit

-- @
--               _ _
--     ___ _   _(_) |_
--    / __| | | | | __|
--    \__ \ |_| | | |_
--    |___/\__,_|_|\__|
--
-- @

-- | Represent a card's suit as a coproduct/sum type
data Suit = Diamonds | Spades | Hearts | Clubs deriving (Enum, Eq)

-- | Define string representation of the suit based on kata rules
--
-- >>> show Diamonds
-- "D"
--
-- >>> show Spades
-- "S"
--
-- >>> show Hearts
-- "H"
--
-- >>> show Clubs
-- "C"
instance Show Suit where
  show Diamonds = "D"
  show Spades   = "S"
  show Hearts   = "H"
  show Clubs    = "C"

-- * Suit

-- @
--                       _
--      ___ __ _ _ __ __| |
--     / __/ _` | '__/ _` |
--    | (_| (_| | | | (_| |
--     \___\__,_|_|  \__,_|
--
-- @

-- | Represent a card as a product of a rank and a suit
data Card = Card CardRank Suit deriving (Eq)

-- | Define string representation of the card based on kata rules
--
-- >>> show $ Card Jack Clubs
-- "JC"
--
-- >>> show $ Card (NumCard 4) Diamonds
-- "4D"
instance Show Card where
  show (Card r s) = show r ++ show s

-- | Defer ordering on the card to just the rank
--
-- Suits are disregarded in comparison:
--
-- >>> (Card (NumCard 5) Spades) > (Card (NumCard 5) Hearts)
-- False
instance Ord Card where
  compare (Card r0 _) (Card r1 _) = compare r0 r1

-- * Hand

-- @
--     _                     _
--    | |__   __ _ _ __   __| |
--    | '_ \ / _` | '_ \ / _` |
--    | | | | (_| | | | | (_| |
--    |_| |_|\__,_|_| |_|\__,_|
--
-- @

-- | Represents a poker hand as a product of cards (five times)
data Hand = Hand Card Card Card Card Card deriving (Eq)

-- * Deck

-- @
--         _           _
--      __| | ___  ___| | __
--     / _` |/ _ \/ __| |/ /
--    | (_| |  __/ (__|   <
--     \__,_|\___|\___|_|\_\
--
-- @

-- | Type alias representing a deck of cards.
type Deck =  [Card]

-- * HandRank

data HandRank =
  -- A high card with the card rank for the high card
  HighCard CardRank
  -- A pair of cards of the given card rank
  | Pair CardRank
  -- A two pair hand rank with high card rank and low card rank
  | TwoPairs CardRank CardRank
  -- A three of a kind hand rank with the card rank
  | ThreeOfAKind CardRank
  -- A straight with the high card rank
  | Straight CardRank
  -- A flush
  | Flush Suit
  -- A full house with the card rank of the triple and the card rank of the double
  | FullHouse CardRank CardRank
  -- A four of a kind hand rank with the card rank of the quadruple
  | FourOfAKind CardRank
  -- A straight flush with the card rank of the highest card, we can infer the rest.
  | StraightFlush CardRank Suit
  deriving (Eq, Show)

instance Ord HandRank where
  compare (StraightFlush r0 _)  (StraightFlush r1 _)  = compare r0 r1
  compare (StraightFlush _ _)   _                     = GT
  compare _                     (StraightFlush _ _)   = LT
  compare (FourOfAKind r0)      (FourOfAKind r1)      = compare r0 r1
  compare (FourOfAKind _)       _                     = GT
  compare _                     (FourOfAKind _)       = LT
  -- TODO: needs love
  compare (FullHouse h0 _)     (FullHouse h1 _)       = compare (fromEnum h0) (fromEnum h1)
  compare (FullHouse _ _)       _                     = GT
  compare _                     (FullHouse _ _)       = LT
  compare (Flush _)             (Flush _)             = EQ
  compare (Flush _)             _                     = GT
  compare _                     (Flush _)             = LT
  compare (Straight r0)         (Straight r1)         = compare r0 r1
  compare (Straight _)          _                     = GT
  compare _                     (Straight _)          = LT
  compare (ThreeOfAKind r0)     (ThreeOfAKind r1)     = compare r0 r1
  compare (ThreeOfAKind _)      _                     = GT
  compare _                     (ThreeOfAKind _)      = LT
  -- TODO: needs love
  compare (TwoPairs p0 _)      (TwoPairs p1 _)        = compare p0 p1
  compare (TwoPairs _ _)        _                     = GT
  compare _                     (TwoPairs _ _)        = LT
  compare (Pair r0)             (Pair r1)             = compare r0 r1
  compare (Pair _)              _                     = GT
  compare _                     (Pair _)              = LT
  compare (HighCard r0)         (HighCard r1)         = compare r0 r1


-- * Predicate

-- @
--                        _ _           _
--     _ __  _ __ ___  __| (_) ___ __ _| |_ ___
--    | '_ \| '__/ _ \/ _` | |/ __/ _` | __/ _ \
--    | |_) | | |  __/ (_| | | (_| (_| | ||  __/
--    | .__/|_|  \___|\__,_|_|\___\__,_|\__\___|
--    |_|
-- @

-- | Type wrapper representing a predicate: given a value of type `a`
--   determine if a condition holds.
newtype Pred a = Pred { unPred :: a -> Bool }


