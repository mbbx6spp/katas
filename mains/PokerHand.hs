{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import           Katas.PokerHand (deck)
import           Prelude         (IO, print, ($))

main :: IO ()
main = print deck
