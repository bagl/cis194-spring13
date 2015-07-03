module HW04.HW04 where

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
        f x (Node _ lT y rT) =
          let hlT = height lT
              hrT = height rT
              lT' = f x lT
              rT' = f x rT
          in if hlT < hrT
             then Node (succ hrT)                  lT' y rT
             else Node (succ $ max hlT (height rT')) lT  y rT'

height :: Tree a -> Integer
height Leaf            = -1
height (Node n _ _ _ ) = n

-- Ex 3

map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []

xor :: [Bool] -> Bool
xor = foldr (\b acc -> if b then not acc else acc) False

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = undefined -- foldr ....

-- Ex 4

-- Given n, copute all od primes up to 2n + 2
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map ((+1) . (*2)) $ filter (not . (`elem` toRemove)) [1..n]
  where toRemove = [ x
                   | j <- [1..div n 2]
                   , i <- [1..j]
                   , let x = i + j + 2 * i * j
                   , x <= n ]

-- ======================================
-- Props
treePropBalanced :: [Int] -> Bool
treePropBalanced xs = isBalanced $ foldTree xs
  where isBalanced Leaf = True
        isBalanced (Node _ left _ right) =
          abs (height left - height right) <= 1
          && isBalanced left
          && isBalanced right

treePropHeight :: [Int] -> Bool
treePropHeight xs =
  case foldTree xs of
    Leaf                  -> True
    (Node h left _ right) ->
      h == 0 || h - 1 == max (height left) (height right)

xorProp :: [Bool] -> Bool
xorProp bs = xor bs == odd (length $ filter id bs)

mapProp' :: [Int] -> Bool
mapProp' xs = map' f xs == map f xs
  where f = (+2)
-- ======================================

main :: IO ()
main = do
  --quickCheck (\xs -> fun1 xs == fun1' xs)
  --print $ map (\n -> fun2 n == fun2' n) [1..20] -- quickCheck not working
  --print $ foldTree ['A'..'J']
  quickCheck treePropHeight
  quickCheck treePropBalanced
  quickCheck xorProp
  quickCheck mapProp'
  print $ take 501 $ sieveSundaram 2000
