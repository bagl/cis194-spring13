module HW06.Fibonacci where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n
  | n > 1     = fib (n - 1) + fib (n - 2)
  | otherwise = error "Cannot call fib with negative number"

fibs1 :: [Integer]
fibs1 = map fib [0..]

fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (drop 1 fibs2)
