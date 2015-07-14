{-# LANGUAGE NoImplicitPrelude #-}

module HW03.Golf (
  skips,
  localMaxima,
  histogram
) where

import StrippedPrelude

import Data.List (tails, group, sort, transpose, init)

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
