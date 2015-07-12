module HW07.Main where

import HW07.JoinList
import HW07.Editor

main :: IO ()
main = runEditor editor $ scoreSizeLine "Testovaci string"
