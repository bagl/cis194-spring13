{-# LANGUAGE FlexibleInstances #-}

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

instance Functor Stream where
  fmap f (Cons a s) = Cons (f a) $ fmap f s

streamToList :: Stream a -> [a]
streamToList (Cons a s) = a : streamToList s

streamRepeat :: a -> Stream a
streamRepeat y = let s = Cons y s
                 in s

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap = fmap

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = Cons a $ streamFromSeed f (f a)

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons a1 s1) (Cons a2 s2) =
  Cons a1 $ Cons a2 $ interleaveStreams s1 s2

nats :: Stream Integer
nats = streamFromSeed (+1) 1

ruler :: Stream Integer
ruler = let f n = Cons n $ interleaveStreams (f $ succ n) (streamRepeat n)
        in f 0 -- #haskell-beginners monochrom copyright
{-
 - couldn't find the way to make this run in following form
 -
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

x :: Stream Integer
x = Cons 0 $ Cons 1 $ streamRepeat 0

instance Num (Stream Integer) where
  fromInteger i = Cons i $ streamRepeat 0
  negate        = fmap negate
  (+) (Cons a0 sa') (Cons b0 sb') =
        Cons (a0 + b0) $ sa' + sb'
  (*) (Cons a0 sa') sb@(Cons b0 sb') =
        Cons (a0 * b0) $ fmap (* a0) sb' + (sa' * sb)
  abs    = undefined
  signum = undefined

instance Fractional (Stream Integer) where
  (/) sa@(Cons a0 sa') sb@(Cons b0 sb') =
        Cons (a0 `div` b0)
        $ fmap (`div` b0) (sa' - (sa / sb) * sb')
  fromRational = undefined

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^(2 :: Int))

data Matrix = Matrix Integer Integer Integer Integer

instance Show Matrix where
  show (Matrix a00 a01 a10 a11) =
    "[ " ++ show a00 ++ " " ++ show a01 ++ " ]\n[ "
         ++ show a10 ++ " " ++ show a11 ++ " ]\n"

instance Num Matrix where
  (Matrix a00 a01 a10 a11) * (Matrix b00 b01 b10 b11) =
    Matrix (a00 * b00 + a01 * b10)
           (a00 * b01 + a01 * b11)
           (a10 * b00 + a11 * b01)
           (a10 * b01 + a11 * b11)
  (-) = undefined
  (+) = undefined
  abs = undefined
  signum = undefined
  fromInteger = undefined

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n | n > 0 = let (Matrix _ a _ _) = Matrix 1 1 1 0 ^ n
                 in a
       | otherwise = error "Cannot call fib with negative number"
