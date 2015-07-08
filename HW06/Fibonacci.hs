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

data Stream a = Cons a (Stream a)

instance Show a => Show (Stream a) where
  show = ("Stream "++) . show . take 20 . streamToList

streamToList :: Stream a -> [a]
streamToList (Cons a s) = a : streamToList s

streamRepeat :: a -> Stream a
streamRepeat x = let s = Cons x s
                 in s

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a s) = Cons (f a) $ streamMap f s

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = Cons a $ streamFromSeed f (f a)

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons a1 s1) (Cons a2 s2) =
  Cons a1 $ Cons a2 $ interleaveStreams s1 s2

nats :: Stream Integer
nats = streamFromSeed (+1) 1

ruler :: Stream Integer
ruler = undefined
{-
  interleaveStreams
    (streamRepeat 0)
    (interleaveStreams
      (streamRepeat 1)
      (interleaveStreams
        (streamRepeat 2)
        (interleaveStreams
          (streamRepeat 3)
          (streamRepeat 4))))
-}
