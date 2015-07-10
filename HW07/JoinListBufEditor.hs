module HW07.Main where

import Data.Monoid (mempty)

import HW07.JoinList
import HW07.Editor

-- TODO: Slow!!!!!!!!!!!!!!!!!
main :: IO ()
main = runEditor editor $ scoreSizeLine "Testovaci string"
