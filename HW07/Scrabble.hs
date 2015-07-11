module HW07.Scrabble where

import Data.Char (toUpper)
import Data.Map (fromList, findWithDefault)
import Data.Monoid (Monoid, mempty, mappend, mconcat)

newtype Score = Score Int deriving (Show, Eq, Ord)

getScore :: Score -> Int
getScore (Score s) = s

instance Monoid Score where
  mempty  = Score 0
  mappend (Score a) (Score b) = Score $ a + b

score :: Char -> Score
score = Score . flip (findWithDefault 0) m . toUpper
  where m = fromList [ ('A',  1), ('B',  3), ('C',  3), ('D',  2)
                     , ('E',  1), ('F',  4), ('G',  2), ('H',  4)
                     , ('I',  1), ('J',  8), ('K',  5), ('L',  1)
                     , ('M',  3), ('N',  1), ('O',  1), ('P',  3)
                     , ('Q', 10), ('R',  1), ('S',  1), ('T',  1)
                     , ('U',  1), ('V',  4), ('W',  4), ('X',  8)
                     , ('Y',  4), ('Z', 10) ]

scoreString :: String -> Score
scoreString = mconcat . map score
