{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HW12.Risk where

import Control.Applicative ((<$>))
import Control.Monad (replicateM, foldM)
import Control.Monad.Random
import Data.List (sortBy, foldl')

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

second :: (b -> c) -> (a, b) -> (a, c)
second f (a, b) = (a, f b)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
  deriving (Show)

dieN :: Int -> Rand StdGen [DieValue]
dieN n = sortBy (flip compare) <$> replicateM n die

limitAttackers :: Army -> Army
limitAttackers = min 3 . pred

limitDefenders :: Army -> Army
limitDefenders = min 2

resolveThrow :: Battlefield -> (DieValue, DieValue) -> Battlefield
resolveThrow (Battlefield as ds) (af, df)
  | af > df   = Battlefield as        (pred ds)
  | otherwise = Battlefield (pred as) ds

-- Ex 2
battle :: Battlefield -> Rand StdGen Battlefield
battle bf = do
  attack <- dieN (limitAttackers $ attackers bf)
  defend <- dieN (limitDefenders $ defenders bf)
  return $ foldl' resolveThrow bf $ zip attack defend

-- Ex 3
invade :: Battlefield -> Rand StdGen Battlefield
invade b = do
  b' <- battle b
  if isFinished b'
  then return b'
  else invade b'
  where isFinished (Battlefield a d) = a < 2 || d == 0

successProb :: Battlefield -> Rand StdGen Double
successProb b = (/ noGames) <$> foldM f 0 [1..noGames]
  where noGames = 100
        f n _ = (n +) . b2n . attackersWin <$> invade b
        attackersWin = (== 0) . defenders
        b2n True  = 1
        b2n False = 0
