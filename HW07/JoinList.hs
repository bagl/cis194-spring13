{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}

module HW07.JoinList where

import Prelude hiding (foldr)

import Data.Foldable (Foldable, foldr)
import Data.Monoid (Monoid, (<>), mempty) --, mconcat, Endo(..), appEndo)

import HW07.Buffer
  ( Buffer
  , toString
  , fromString
  , line
  , replaceLine
  , numLines
  , value )
import HW07.Scrabble
import HW07.Sized

type JLScoreBuffer     = JoinList Score String
type JLSizeBuffer      = JoinList Size String
type JLScoreSizeBuffer = JoinList (Score, Size) String

-- Ex 1
--
data JoinList m a = Empty
                | Single m a
                | Append m (JoinList m a) (JoinList m a)
                deriving (Eq, Show)

empty :: JoinList m a
empty = Empty

single :: (a -> b) -> a -> JoinList b a
single = (Single =<<)

instance Functor (JoinList m) where
  fmap _ Empty            = Empty
  fmap f (Single m a)     = Single m $ f a
  fmap f (Append m l1 l2) = Append m (fmap f l1) (fmap f l2)

instance Foldable (JoinList m) where
  foldr _ z Empty = z
  foldr f z (Single _ x) = f x z
  foldr f z (Append _ l1 l2) = foldr f (foldr f z l2) l1

instance (Sized m, Monoid m) => Sized (JoinList m a) where
  size = size . tag

-- Non-associative, right?
-- =======================
--instance (Monoid m, Monoid a) => Monoid (JoinList m a) where
--  mempty = Empty
--  mappend l1 l2 = Append (tag l1 <> tag l2) l1 l2

tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m _)   = m
tag (Append m _ _) = m

(+++) :: Monoid m
      => JoinList m a
      -> JoinList m a
      -> JoinList m a
(+++) Empty l = l
(+++) l Empty = l
(+++) l1 l2   = Append (tag l1 <> tag l2) l1 l2

-- Ex 2
--

jlSize :: (Sized b, Monoid b) => JoinList b a -> Int
jlSize = getSize . size

indexJ :: (Sized b, Monoid b)
       => Int
       -> JoinList b a
       -> Maybe a
indexJ i l
  | i < 0   = Nothing
  | i >= il = Nothing
  where il  = jlSize l
indexJ i (Append _ l1 l2)
  | i < il1   = indexJ i l1
  | otherwise = indexJ (i - il1) l2
  where il1 = jlSize l1
indexJ i l@(Single _ a)
  | i == jlSize l - 1 = Just a
indexJ _ _ = Nothing

dropJ :: (Sized b, Monoid b)
      => Int
      -> JoinList b a
      -> JoinList b a
dropJ n l
  | n <= 0  = l
  | n >= il = Empty
  where il  = jlSize l
dropJ n (Append _ l1 l2)
  | n <= il1  = dropJ n l1 +++ l2
  | otherwise = dropJ (n - il1) l2
  where il1 = jlSize l1
dropJ _ _ = Empty

takeJ :: (Sized b, Monoid b)
      => Int
      -> JoinList b a
      -> JoinList b a
takeJ n l
  | n <= 0  = Empty
  | n >= il = l
  where il  = jlSize l
takeJ n (Append _ l1 l2)
  | n <= il1  = takeJ n l1
  | otherwise = l1 +++ takeJ (n - il1) l2
  where il1 = jlSize l1
takeJ _ _ = Empty

splitAtJ :: (Sized b, Monoid b)
         => Int
         -> JoinList b a
         -> (JoinList b a, JoinList b a)
splitAtJ n b = (takeJ n b, dropJ n b)

jToList :: JoinList b a -> [a]
jToList = foldr (:) []

jFromList :: Monoid b => (a -> b) -> [a] -> JoinList b a
jFromList f = g =<< length
  where g _ []  = Empty
        g _ [x] = Single (f x) x
        g n xs  = let n1 = n `div` 2
                      (lh, rh) = splitAt n1 xs
                  in g n1 lh +++ g (n-n1) rh

scoreSize :: Scored a => a -> (Score, Size)
scoreSize = (, Size 1) . score

singleSize :: a -> JoinList Size a
singleSize = single $ const (Size 1)

singleScore :: Scored a => a -> JoinList Score a
singleScore = single score

singleScoreSize :: Scored a => a -> JoinList (Score, Size) a
singleScoreSize = single scoreSize

scoreLine :: String -> JLScoreBuffer
scoreLine [] = Empty
scoreLine s  = singleScore s

scoreSizeLine :: String -> JLScoreSizeBuffer
scoreSizeLine [] = Empty
scoreSizeLine s = singleScoreSize s

instance Buffer JLScoreSizeBuffer where
  toString = foldr (++) "" -- appEndo (foldr ((<>) . Endo . (++)) mempty l) ""
  fromString = jFromList scoreSize . lines
  line = indexJ
  replaceLine n s b = let (l1, l2) = splitAtJ n b
                      in l1 +++ scoreSizeLine s +++ dropJ 1 l2
  numLines = getSize . size
  value = getScore . fst . tag

(!!?) :: [a] -> Int -> Maybe a
(!!?) [] _        = Nothing
(!!?) _ i | i < 0 = Nothing
(!!?) (x:_) 0     = Just x
(!!?) (_:xs) i    = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2
