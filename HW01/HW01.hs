module HW01 where

import Data.Char (digitToInt)

-- Exercise 1
toDigitsRev :: Integer -> [Integer]
toDigitsRev d
  | d <= 0    = []
  | otherwise = rem d 10 : toDigitsRev (quot d 10)

toDigitsRev' :: Integer -> [Integer]
toDigitsRev' d
  | d <= 0    = []
  | otherwise =  map (fromIntegral . digitToInt) $ show d

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

-- Exercise 2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther (x:y:ys) = x : 2*y : doubleEveryOther ys
doubleEveryOther xs       = xs

-- Exercise 3
sumDigits :: [Integer] -> Integer
sumDigits = sum . concatMap toDigitsRev

-- Exercise 4
validate :: Integer -> Bool
validate = (0 ==) . (`rem` 10) . sumDigits . doubleEveryOther . toDigitsRev

-- Exercise 5
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n-1) a c b ++ [(a,b)] ++ hanoi (n-1) c b a

-----------------------------------------------------------------------
main :: IO ()
main = do
  print $ validate 4012888888881882 -- False
  print $ hanoi 3 "a" "b" "c"
