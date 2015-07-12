{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Control.Applicative ((<$>), (<*>))
import Test.QuickCheck
import Test.QuickCheck.Function

import HW07.JoinList
import HW07.Sized

-- =============================================================
-- HW07
-- =============================================================
instance (Arbitrary a) => Arbitrary (JoinList Size a) where
  arbitrary =
    frequency [ (1, return empty)
              , (3, singleSize <$> arbitrary)
              , (3, (+++) <$> arbitrary <*> arbitrary)
              ]

data JLWithIndex = JLWithIndex Int (JoinList Size Int) deriving Show
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

prop_jToList :: JoinList Size Int -> Bool
prop_jToList jl = jlToList jl == jToList jl

prop_indexJ :: JLWithIndex -> Bool
prop_indexJ (JLWithIndex i jl) = indexJ i jl == jlToList jl !!? i

prop_takeJ :: JLWithIndex -> Bool
prop_takeJ (JLWithIndex n jl) =
  jlToList (takeJ n jl) == take n (jlToList jl)

prop_dropJ :: JLWithIndex -> Bool
prop_dropJ (JLWithIndex n jl) =
  jlToList (dropJ n jl) == drop n (jlToList jl)

prop_splitAtJ :: Int -> JoinList Size Int -> Bool
prop_splitAtJ i jl =
  let (l1, l2) = splitAtJ i jl
  in (jToList l1, jToList l2) == splitAt i (jToList jl)
-- =============================================================

main :: IO ()
main = do
  -- =========================
  -- HW07
  -- =========================
  quickCheck (prop_functorLaw1 :: JoinList Size Int -> Bool)
  quickCheck (prop_functorLaw2 :: JoinList Size Int -> Fun Int Int -> Fun Int Int -> Bool)
  quickCheck prop_jToList
  quickCheck prop_indexJ
  quickCheck prop_takeJ
  quickCheck prop_dropJ
  quickCheck prop_splitAtJ
  -- =========================
