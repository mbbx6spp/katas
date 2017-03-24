# Katas

Some code for some katas (so far just partial implementation of
the PokerHands kata) with the goal of documenting different approaches
and highlighting good and bad things about different parts of the
solutions.

1. *extension*: using the core poker game rules (to reuse them) in a
   Texas Hold 'em poker game since the latter is a super set of the former.

2. *decomposition+composition*: how we can define simple building blocks that
   can be combined in different ways.

3. *randomization*: for shuffling the deck of cards and/or "dealing" hands in
   a game we need to model randomness which is very non-FP. So how do we
   program randomness in Haskell? :)

4. *tournament*: modeling a poker tournament to traverse match ups, etc. in a
   way that improves comprehension and/or reduces logic errors.
