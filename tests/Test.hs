{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Control.Applicative ((<$>), (<*>))
import Data.List (tails)
import Test.QuickCheck
import Test.QuickCheck.Function

import HW03.Golf
import HW04.HW04
import HW07.JoinList
import HW07.Scrabble
import HW07.Sized

-- =============================================================
-- HW03
-- =============================================================
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

-- =============================================================
-- HW04
-- =============================================================
prop_treeBalanced :: [Int] -> Bool
prop_treeBalanced xs = isBalanced $ foldTree xs
  where isBalanced Leaf = True
        isBalanced (Node _ left _ right) =
          abs (height left - height right) <= 1
          && isBalanced left
          && isBalanced right

prop_treeHeight :: [Int] -> Bool
prop_treeHeight xs =
  case foldTree xs of
    Leaf                  -> True
    (Node h left _ right) -> h - 1 == max (height left) (height right)

prop_xor :: [Bool] -> Bool
prop_xor bs = xor bs == odd (length $ filter id bs)

prop_map :: [Int] -> Bool
prop_map xs = map' fun xs == map fun xs
  where fun = (+2)

prop_fun1 :: [Integer] -> Bool
prop_fun1 xs = fun1 xs == fun1' xs

prop_fun2 :: Positive Integer -> Bool
prop_fun2 (Positive n) = fun2 n == fun2' n

prop_foldl :: [Integer] -> Bool
prop_foldl xs = foldl fun [] xs == myFoldl fun [] xs
  where fun = flip (:)

-- =============================================================
-- HW07
-- =============================================================
instance (Arbitrary a, Scored a) => Arbitrary (JoinList (Score, Size) a) where
  arbitrary =
    frequency [ (1, return empty)
              , (3, singleScoreSize <$> arbitrary)
              , (3, (+++) <$> arbitrary <*> arbitrary)
              ]

data JLWithIndex = JLWithIndex Int (JoinList (Score, Size) Int) deriving Show
instance Arbitrary JLWithIndex where
  arbitrary = do
    jl <- arbitrary
    n  <- elements [-1 .. jlSize jl + 1]
    return $ JLWithIndex n jl

prop_functorLaw1 :: (Functor f, Eq (f a))
                  => f a
                  -> Bool
prop_functorLaw1 f = fmap id f == id f

prop_functorLaw2 :: (Functor f, Eq (f c))
                 => f a
                 -> Fun b c
                 -> Fun a b
                 -> Bool
prop_functorLaw2 x (apply -> f) (apply -> g) =
  fmap (f . g) x == (fmap f . fmap g) x

prop_jToList :: JoinList (Score, Size) Int -> Bool
prop_jToList jl = jlToList jl == jToList jl

prop_indexJ :: JLWithIndex -> Bool
prop_indexJ (JLWithIndex i jl) = indexJ i jl == jlToList jl !!? i

prop_takeJ :: JLWithIndex -> Bool
prop_takeJ (JLWithIndex n jl) =
  jlToList (takeJ n jl) == take n (jlToList jl)

prop_dropJ :: JLWithIndex -> Bool
prop_dropJ (JLWithIndex n jl) =
  jlToList (dropJ n jl) == drop n (jlToList jl)

prop_splitAtJ :: Int -> JoinList (Score, Size) Int -> Bool
prop_splitAtJ i jl =
  let (l1, l2) = splitAtJ i jl
  in (jToList l1, jToList l2) == splitAt i (jToList jl)
-- =============================================================

main :: IO ()
main = do
  -- =========================
  -- HW03
  -- =========================
  quickCheck prop_skipsLength
  quickCheck prop_skipsLengths
  quickCheck prop_skipsFirst
  quickCheck prop_localMaximaLength
  quickCheck prop_localMaximaElems
  putStrLn $ histogram [2,5,4,4,2,1,8,1,2,4,1,2,4,8]
  -- =========================
  -- HW04
  -- =========================
  quickCheck prop_fun1
  quickCheck prop_fun2
  quickCheck prop_treeBalanced
  quickCheck prop_treeHeight
  quickCheck prop_map
  quickCheck prop_xor
  quickCheck prop_foldl
  print $ take 501 $ sieveSundaram 2000
  -- =========================
  -- HW07
  -- =========================
  quickCheck (prop_functorLaw1 :: JoinList (Score, Size) Int -> Bool)
  quickCheck (prop_functorLaw2 :: JoinList (Score, Size) Int -> Fun Int Int -> Fun Int Int -> Bool)
  quickCheck prop_jToList
  quickCheck prop_indexJ
  quickCheck prop_takeJ
  quickCheck prop_dropJ
  quickCheck prop_splitAtJ
  -- =========================
