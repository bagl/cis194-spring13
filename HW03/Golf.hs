{-# LANGUAGE NoImplicitPrelude #-}

module HW03.Golf where

import StrippedPrelude

import Data.List (tails, group, sort, tail, transpose, init)
import Test.QuickCheck

skips :: [a] -> [[a]]
skips = zipWith f [0..] . init . tails

f :: Int -> [a] -> [a]
f _ []      = []
f n (y:ys)  = y : f n (drop n ys)

localMaxima :: [Integer] -> [Integer]
localMaxima l = [ y | (x:y:z:_) <- tails l, y > max x z ]

pad :: Int -> String -> String
pad n a = a ++ replicate (n - length a) ' '

histogram :: [Int] -> String
histogram =
  (++ "==========\n0123456789\n")
  . unlines
  . reverse
  . tail
  . transpose
  . uncurry (\n -> map (pad n . map (const '*')))
  . ((,) =<< maximum . map length)
  . group
  . sort
  . (++[0..9])

main :: IO ()
main = do
  quickCheck prop_skipsLength
  quickCheck prop_skipsLengths
  quickCheck prop_skipsFirst

  quickCheck prop_localMaximaLength
  quickCheck prop_localMaximaElems
  where
    prop_skipsLength :: [Int] -> Bool
    prop_skipsLength xs =
      length xs == length (skips xs)

    prop_skipsLengths :: [Int] -> Bool
    prop_skipsLengths xs =
      map length (skips xs) == map (length xs `div`) [1..length xs]

    prop_skipsFirst :: [Int] -> Bool
    prop_skipsFirst xs =
      xs == map (\(x:_) -> x) (skips xs)

    prop_localMaximaLength :: [Integer] -> Bool
    prop_localMaximaLength xs =
      length (localMaxima xs) <= length xs `div` 2

    prop_localMaximaElems :: [Integer] -> Bool
    prop_localMaximaElems xs =
      localMaxima xs == concatMap g (tails xs)
      where
        g :: [Integer] -> [Integer]
        g (x:y:z:_) | y > max x z = [y]
        g _                       = []
