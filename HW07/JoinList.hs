{-# LANGUAGE FlexibleInstances #-}

module HW07.JoinList where

import Prelude hiding (foldr)

import Data.Foldable (Foldable, foldr)
import Data.Monoid (Monoid, (<>), mempty)
--import Test.QuickCheck (quickCheck, Positive(..))

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


-- Ex 1
--
data JoinList m a = Empty
                | Single m a
                | Append m (JoinList m a) (JoinList m a)
                deriving (Eq, Show)

instance Functor (JoinList m) where
  fmap _ Empty            = Empty
  fmap f (Single m a)     = Single m $ f a
  fmap f (Append m l1 l2) = Append m (fmap f l1) (fmap f l2)

instance Foldable (JoinList m) where
  foldr _ z Empty = z
  foldr f z (Single _ x) = f x z
  foldr f z (Append _ l1 l2) = foldr f (foldr f z l2) l1

tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m _)   = m
tag (Append m _ _) = m

(+++) :: Monoid m
      => JoinList m a
      -> JoinList m a
      -> JoinList m a
(+++) Empty Empty = Empty
(+++) Empty l = l
(+++) l Empty = l
(+++) l1 l2 = Append (tag l1 <> tag l2) l1 l2

-- Ex 2
--
jlIndex :: (Monoid m, Sized m) => JoinList m a -> Int
jlIndex = getSize . size . tag

indexJ :: (Sized b, Monoid b)
       => Int
       -> JoinList b a
       -> Maybe a
indexJ _ Empty = Nothing
indexJ i l@(Single _ a)
  | i == jlIndex l = Just a
  | otherwise      = Nothing
indexJ i l@(Append _ l1 l2)
  | i >  il   = Nothing
  | i <= il1  = indexJ i l1
  | i >  il1  = indexJ (i - il1) l2
  | otherwise = Nothing -- should not happen
  where il  = jlIndex l
        il1 = jlIndex l1

dropJ :: (Sized b, Monoid b)
      => Int
      -> JoinList b a
      -> JoinList b a
dropJ n l
  | n <= 0       = l
  | n >= il      = Empty
  where il  = jlIndex l
dropJ _ Empty{}  = Empty
dropJ _ Single{} = Empty
dropJ n (Append _ l1 l2)
  | n <= il1  = dropJ n l1 +++ l2
  | otherwise = dropJ (n - il1) l2
  where il1 = jlIndex l1

takeJ :: (Sized b, Monoid b)
      => Int
      -> JoinList b a
      -> JoinList b a
takeJ n l
  | n <= 0       = Empty
  | n >= il      = l
  where il  = jlIndex l
takeJ _ Empty{}  = Empty
takeJ n l@Single{}
  | n >= il      = l
  | otherwise    = Empty
  where il = jlIndex l
takeJ n (Append _ l1 l2)
  | n <= il1  = takeJ n l1
  | otherwise = l1 +++ takeJ (n - il1) l2
  where il1 = jlIndex l1

scoreLine :: String -> JoinList Score String
scoreLine [] = Empty
scoreLine s  = Single =<< scoreString $ s

scoreSizeLine :: String -> JoinList (Score, Size) String
scoreSizeLine [] = Empty
scoreSizeLine s = Single (scoreString s, Size 1) s

instance Buffer (JoinList (Score, Size) String) where
  toString = foldr (++) ""
  fromString = foldr ((+++) . scoreSizeLine) Empty . lines
  line = indexJ
  replaceLine n s b = takeJ (n-1) b +++ scoreSizeLine s +++ dropJ n b
  numLines = getSize . snd . tag
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

--main :: IO ()
--main = do
--  --quickCheck prop_indexJ
--  undefined
--
--  where
--    prop_indexJ :: (Positive Int) -> JoinList Size Int -> Bool
--    prop_indexJ (Positive i) jl = indexJ i jl == jlToList jl !!? i
