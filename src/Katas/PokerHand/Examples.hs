-- | Module full of examples of hands and their rankings
module Katas.PokerHand.Examples where

import           Data.List                 (reverse, sort, (++))
import           Katas.PokerHand.Functions
import           Katas.PokerHand.Types
import           Prelude                   (Ordering, compare, dropWhile, zip,
                                            ($), (==))

-- Black: 2H 3D 5S 9C KD  White: 2C 3H 4S 8C AH
black0 :: Hand
black0 = Hand c0 c1 c2 c3 c4
  where
    c0 = Card (NumCard 2) Hearts
    c1 = Card (NumCard 3) Diamonds
    c2 = Card (NumCard 5) Spades
    c3 = Card (NumCard 9) Clubs
    c4 = Card King Diamonds

white0 :: Hand
white0 = Hand c0 c1 c2 c3 c4
  where
    c0 = Card (NumCard 2) Clubs
    c1 = Card (NumCard 3) Hearts
    c2 = Card (NumCard 4) Spades
    c3 = Card (NumCard 8) Clubs
    c4 = Card Ace Hearts

-- | Represents first game.
-- TODO: Take out the sorts
game0 :: [(HandRank, HandRank)]
game0 = dropWhile pairWiseEqual $ blackHandRanks `zip` whiteHandRanks
  where
    pairWiseEqual (x, y) = x == y
    blackHandRanks = sort $ toHandRanks black0
    whiteHandRanks = sort $ toHandRanks white0

-- Black: 2H 4S 4C 2D 4H  White: 2S 8S AS QS 3S
-- Black: 2H 3D 5S 9C KD  White: 2C 3H 4S 8C KH
-- Black: 2H 3D 5S 9C KD  White: 2D 3H 5C 9S KH
