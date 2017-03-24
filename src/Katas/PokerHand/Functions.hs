{-# LANGUAGE NoImplicitPrelude #-}

-- | Functions that support the PokerHand kata solution
module Katas.PokerHand.Functions where

import           Data.List             (elem, filter, group, maximum, minimum,
                                        nub, reverse, sort, (++))
import           Katas.PokerHand.Types
import           Prelude               (Int, fmap, fromEnum, length, maxBound,
                                        minBound, undefined, ($), (&&), (-),
                                        (.), (==), (||))

-- * Predicate Algebra

-- @
--                       _ _           _               _            _
--    _ __  _ __ ___  __| (_) ___ __ _| |_ ___    __ _| | __ _  ___| |__  _ __ __ _
--   | '_ \| '__/ _ \/ _` | |/ __/ _` | __/ _ \  / _` | |/ _` |/ _ \ '_ \| '__/ _` |
--   | |_) | | |  __/ (_| | | (_| (_| | ||  __/ | (_| | | (_| |  __/ |_) | | | (_| |
--   | .__/|_|  \___|\__,_|_|\___\__,_|\__\___|  \__,_|_|\__, |\___|_.__/|_|  \__,_|
--   |_|                                                 |___/
-- @

-- | Higher order function representing AND-ing two predicates of the same given type.
--
-- >>> import Prelude (odd, even)
-- >>> let p0 = Pred odd
-- >>> let p1 = Pred even
-- >>> let p2 = p0 &&& p1
-- >>> (unPred p2) 6
-- False
-- >>> (unPred p2) 7
-- False
--
-- The type below is a synonym to:
-- @
--   (&&&) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
-- @
(&&&) :: Pred a -> Pred a -> Pred a
(&&&) (Pred unP0) (Pred unP1) = Pred (\a -> unP0 a && unP1 a)

-- | Higher order function representing OR-ing two predicates of the same given type.
--
-- >>> import Prelude (odd, even)
-- >>> let p0 = Pred odd
-- >>> let p1 = Pred even
-- >>> let p2 = p0 ||| p1
-- >>> (unPred p2) 6
-- True
-- >>> (unPred p2) 7
-- True
--
-- The type below is a synonym to:
-- @
--   (|||) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
-- @
(|||) :: Pred a -> Pred a -> Pred a
(|||) (Pred unP0) (Pred unP1) = Pred (\a -> unP0 a || unP1 a)


-- * Poker algebra

-- @
--                _                     _            _
--    _ __   ___ | | _____ _ __    __ _| | __ _  ___| |__  _ __ __ _
--   | '_ \ / _ \| |/ / _ \ '__|  / _` | |/ _` |/ _ \ '_ \| '__/ _` |
--   | |_) | (_) |   <  __/ |    | (_| | | (_| |  __/ |_) | | | (_| |
--   | .__/ \___/|_|\_\___|_|     \__,_|_|\__, |\___|_.__/|_|  \__,_|
--   |_|                                  |___/
-- @

-- | Given a card returns its card rank.
getCardRank :: Card -> CardRank
getCardRank (Card r _) = r

-- | Given a hand returns a list of card ranks for each card in the hand.
cardRanks :: Hand -> [CardRank]
cardRanks hand = fmap getCardRank $ toList hand

-- | Given a card returns its card suit.
getCardSuit :: Card -> Suit
getCardSuit (Card _ s) = s

-- | Given a hand returns a list of card suits for each card in the hand.
cardSuits :: Hand -> [Suit]
cardSuits hand = fmap getCardSuit $ toList hand

-- | Given a hand compute the list of lengths of same card ranks
dupList :: Hand -> [Int]
dupList hand = fmap length $ group $ sort $ cardRanks hand

-- | Given a hand compute all the hand ranks possible
-- TODO: implement
toHandRanks :: Hand -> [HandRank]
toHandRanks = undefined

-- | Given a hand compute all the high card ranks possible
--
highCardRanking :: Hand -> [HandRank] -> [HandRank]
highCardRanking hand seed = (fmap (HighCard . getCardRank) $ (reverse . sort) $ toList hand) ++ seed

-- | Convert a hand to a list of cards
--
-- >>> let c0 = Card (NumCard 7) Hearts
-- >>> let c1 = Card (NumCard 7) Diamonds
-- >>> let c2 = Card Jack Spades
-- >>> let c3 = Card (NumCard 8) Clubs
-- >>> let c4 = Card (NumCard 9) Clubs
-- >>> toList $ Hand c0 c1 c2 c3 c4
-- [7H,7D,JS,8C,9C]
toList :: Hand -> [Card]
toList (Hand c0 c1 c2 c3 c4) = [c0, c1, c2, c3, c4]

-- | A deck of cards as a list of Cards
deck :: Deck
deck = [ Card r s | r <- [min .. max], s <- [Diamonds, Spades, Hearts, Clubs] ]
  where
    min = minBound :: CardRank
    max = maxBound :: CardRank

-- | Shuffles a deck of cards into a new order
-- TODO: Implement with randomization.
shuffle :: Deck -> Deck
shuffle = undefined

-- | Number of cards per hand in a poker game
cardsPerHand :: Int
cardsPerHand = 5

-- | Given a number of cards to check for card rank equality, produce a
--   predicate that determines if a poker hand has N of a kind (card rank).
--
-- >>> let cr = NumCard 2
-- >>> let c0 = Card cr Diamonds
-- >>> let c1 = Card cr Hearts
-- >>> let c2 = Card cr Spades
-- >>> let c3 = Card Jack Hearts
-- >>> let c4 = Card Ace Clubs
-- >>> unPred (isNOfAKind 3) (Hand c0 c1 c2 c3 c4)
-- True
--
-- >>> let cr = NumCard 2
-- >>> let c0 = Card cr Diamonds
-- >>> let c1 = Card cr Hearts
-- >>> let c2 = Card cr Spades
-- >>> let c3 = Card Jack Hearts
-- >>> let c4 = Card cr Clubs
-- >>> unPred (isNOfAKind 3) (Hand c0 c1 c2 c3 c4)
-- False
--
-- >>> let cr = NumCard 2
-- >>> let c0 = Card cr Diamonds
-- >>> let c1 = Card cr Hearts
-- >>> let c2 = Card cr Spades
-- >>> let c3 = Card Jack Hearts
-- >>> let c4 = Card cr Clubs
-- >>> unPred (isNOfAKind 4) (Hand c0 c1 c2 c3 c4)
-- True
isNOfAKind :: Int -> Pred Hand
isNOfAKind n = Pred $ \hand ->  n `elem` (fmap length $ group $ sort $ cardRanks hand)

-- | Predicate for two of a kind.
isTwoOfAKind :: Pred Hand
isTwoOfAKind = isNOfAKind 2

-- | Predicate for three of a kind.
isThreeOfAKind :: Pred Hand
isThreeOfAKind = isNOfAKind 3

-- | Predicate for four of a kind.
isFourOfAKind :: Pred Hand
isFourOfAKind = isNOfAKind 4

-- | Predicate that determines if a hand contains all the same suit. A flush.
--
-- >>> let cs = Diamonds
-- >>> let c0 = Card Jack cs
-- >>> let c1 = Card Queen cs
-- >>> let c2 = Card King cs
-- >>> let c3 = Card Ace cs
-- >>> let c4 = Card (NumCard 10) cs
-- >>> unPred isFlush (Hand c0 c1 c2 c3 c4)
-- True
isFlush :: Pred Hand
isFlush = Pred $ \hand -> length (nub $ cardSuits hand) == 1

-- | Predicate that determines if a hand contains distinct card ranks.
isDistinctByRank :: Pred Hand
isDistinctByRank = Pred (\hand -> length (nub $ cardRanks hand) == cardsPerHand)

-- | Predicate that determines if a hand contains a spread of card ranks by
--   exactly a given number.
--
-- >>> let cs = Diamonds
-- >>> let c0 = Card Jack cs
-- >>> let c1 = Card Queen cs
-- >>> let c2 = Card King cs
-- >>> let c3 = Card Ace cs
-- >>> let c4 = Card (NumCard 10) cs
-- >>> unPred (isSpreadBy 4) (Hand c0 c1 c2 c3 c4)
-- True
isSpreadBy :: Int -> Pred Hand
isSpreadBy n = Pred $ \hand -> (mx hand - mn hand) == n
  where
    mn :: Hand -> Int
    mn hand = minimum $ fmap fromEnum $ cardRanks hand
    mx :: Hand -> Int
    mx hand = maximum $ fmap fromEnum $ cardRanks hand

-- | Predicate that determines if a hand is straight, i.e. when ordered from
--   lowest to highest, the card rank scores are sequential.
--
-- >>> let c0 = Card Jack Diamonds
-- >>> let c1 = Card Queen Hearts
-- >>> let c2 = Card King Clubs
-- >>> let c3 = Card Ace Clubs
-- >>> let c4 = Card (NumCard 10) Spades
-- >>> unPred isStraight (Hand c0 c1 c2 c3 c4)
-- True
isStraight :: Pred Hand
isStraight = (isSpreadBy 4) &&& isDistinctByRank

-- | Predicate that determines if a hand is a straight flush, i.e. when
--   ordered from lowest to highest, the card rank scores are sequential and
--   the suit for all the cards in the hand is the same.
--
-- >>> let c0 = Card Jack Diamonds
-- >>> let c1 = Card Queen Hearts
-- >>> let c2 = Card King Clubs
-- >>> let c3 = Card Ace Clubs
-- >>> let c4 = Card (NumCard 10) Spades
-- >>> unPred isStraightFlush (Hand c0 c1 c2 c3 c4)
-- False
--
-- >>> let cs = Hearts
-- >>> let c0 = Card Jack cs
-- >>> let c1 = Card Queen cs
-- >>> let c2 = Card King cs
-- >>> let c3 = Card Ace cs
-- >>> let c4 = Card (NumCard 10) cs
-- >>> unPred isStraightFlush (Hand c0 c1 c2 c3 c4)
-- True
isStraightFlush :: Pred Hand
isStraightFlush = isStraight &&& isFlush

-- | Predicate that determines if a hand is a full house
--
-- >>> let c0 = Card Jack Diamonds
-- >>> let c1 = Card Jack Hearts
-- >>> let c2 = Card King Clubs
-- >>> let c3 = Card Ace Clubs
-- >>> let c4 = Card Ace Spades
-- >>> unPred isFullHouse (Hand c0 c1 c2 c3 c4)
-- False
--
-- >>> let c0 = Card Jack Diamonds
-- >>> let c1 = Card Jack Hearts
-- >>> let c2 = Card Jack Clubs
-- >>> let c3 = Card Ace Clubs
-- >>> let c4 = Card Ace Spades
-- >>> unPred isFullHouse (Hand c0 c1 c2 c3 c4)
-- True
isFullHouse :: Pred Hand
isFullHouse = isNOfAKind 3 &&& isNOfAKind 2

-- | Predicate that determines if a hand has two pairs
--
-- >>> let c0 = Card Jack Diamonds
-- >>> let c1 = Card Jack Hearts
-- >>> let c2 = Card King Clubs
-- >>> let c3 = Card Ace Clubs
-- >>> let c4 = Card Ace Spades
-- >>> unPred isTwoPairs (Hand c0 c1 c2 c3 c4)
-- True
--
-- >>> let c0 = Card (NumCard 10) Diamonds
-- >>> let c1 = Card Jack Hearts
-- >>> let c2 = Card King Clubs
-- >>> let c3 = Card Ace Clubs
-- >>> let c4 = Card Ace Spades
-- >>> unPred isTwoPairs (Hand c0 c1 c2 c3 c4)
-- False
--
isTwoPairs :: Pred Hand
isTwoPairs = Pred $ \hand -> 2 == (length $ filter (== 2) $ dupList hand)
