module HW04.HW04 where

import Data.List ((\\))
import Test.QuickCheck

-- Ex 1

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs) | even x    = (x - 2) * fun1 xs
            | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n    = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (/= 1) . iterate next
  where
    next :: Integer -> Integer
    next n | even n    = n `div` 2
           | otherwise = 3 * n + 1

-- Ex 2

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr f Leaf
  where f :: a -> Tree a -> Tree a
        f x Leaf = Node 0 Leaf x Leaf
        f x (Node h lT y rT) =
          let hlT = height lT
              hrT = height rT
              lT' = f x lT
              rT' = f x rT
              hNew = succ (height rT')
          in if hlT < hrT
             then Node h    lT' y rT
             else Node hNew lT  y rT'

height :: Tree a -> Integer
height Leaf            = -1
height (Node n _ _ _ ) = n

-- Ex 3

map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []

xor :: [Bool] -> Bool
xor = foldr (\b acc -> if b then not acc else acc) False

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (\x h -> h . (`f` x)) id xs base

-- Ex 4

-- Given n, copute all od primes up to 2n + 2
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map ((+1) . (*2)) $ [1..n] \\ toRemove
  where toRemove = [ x
                   | j <- [1..div n 2]
                   , i <- [1..j]
                   , let x = i + j + 2 * i * j
                   , x <= n ]

-- ======================================
-- Props
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
prop_map xs = map' f xs == map f xs
  where f = (+2)

prop_fun1 :: [Integer] -> Bool
prop_fun1 xs = fun1 xs == fun1' xs

prop_fun2 :: Positive Integer -> Bool
prop_fun2 (Positive n) = fun2 n == fun2' n

prop_foldl :: [Integer] -> Bool
prop_foldl xs = foldl f [] xs == myFoldl f [] xs
  where f = flip (:)
-- ======================================

main :: IO ()
main = do
  quickCheck prop_fun1
  quickCheck prop_fun2
  quickCheck prop_treeBalanced
  quickCheck prop_treeHeight
  quickCheck prop_map
  quickCheck prop_xor
  quickCheck prop_foldl
  print $ take 501 $ sieveSundaram 2000
