{-# LANGUAGE NoImplicitPrelude #-}

module HW03.Golf where

import StrippedPrelude

import Data.List (tails, group, sort, transpose, init)
import Test.QuickCheck

skips :: [a] -> [[a]]
skips = zipWith f [0..] . init . tails -- `init` cannot fail because of `tails`

f :: Int -> [a] -> [a]
f n (y:ys) = y : f n (drop n ys)
f _ _      = []

localMaxima :: [Integer] -> [Integer]
localMaxima l = [ y | (x:y:z:_) <- tails l, y > max x z ] -- if pattern matching fails, there cannot be any local maximum

pad :: Int -> String -> String
pad n s = replicate (n - length s) ' ' ++ s

histogram :: [Int] -> String
histogram =
  (++ "==========\n0123456789\n") -- append labels
  . unlines
  . init      -- trim row full of '*' that was added with ([0..9]++) and is always there (so init cannot fail)
  . transpose -- make '*' go horizontally, instead of vertically
  . uncurry (\n -> map (pad n . map (const '*'))) -- map elements of groups to '*' and pad them to length of a biggest group
  . ((,) =<< maximum . map length) -- === maximum . map length >>= (,)
                                   -- === \xss -> (,) (maximum . map length $ xss) xss
                                   -- === \xss -> (maximum $ map length xss, xss)
                                   --
                                   -- returns length of a biggest group and the whole grouping so it can be further worked on
  . group  -- [[0, ...], [1, ...], [2, ...] ... [9, ...]]
  . sort
  . ([0..9]++)
  -- . filter (`elem` [0..9]) -- if we need to take care of bad input

main :: IO ()
main = do
  quickCheck prop_skipsLength
  quickCheck prop_skipsLengths
  quickCheck prop_skipsFirst

  quickCheck prop_localMaximaLength
  quickCheck prop_localMaximaElems

  putStrLn $ histogram [2,5,4,4,2,1,8,1,2,4,1,2,4,8]
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
