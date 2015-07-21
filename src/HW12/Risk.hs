{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HW12.Risk where

import StrippedPrelude
import Control.Applicative ((<$>))
import Control.Monad (replicateM)
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
dieN n = sortDesc <$> replicateM n die
  where sortDesc = sortBy (flip compare)

limitAttackers :: Battlefield -> Army
limitAttackers = min 3 . pred . attackers

limitDefenders :: Battlefield -> Army
limitDefenders = min 2 . defenders

killAttacker :: Battlefield -> Battlefield
killAttacker (Battlefield as ds) = Battlefield (pred as) ds

killDefender :: Battlefield -> Battlefield
killDefender (Battlefield as ds) = Battlefield as (pred ds)

resolveThrow :: Battlefield -> (DieValue, DieValue) -> Battlefield
resolveThrow bf (af, df)
  | af > df   = killDefender bf
  | otherwise = killAttacker bf

-- Ex 2
battle :: Battlefield -> Rand StdGen Battlefield
battle bf = do
  attack <- dieN $ limitAttackers bf
  defend <- dieN $ limitDefenders bf
  return $ foldl' resolveThrow bf $ zip attack defend

-- Ex 3
invade :: Battlefield -> Rand StdGen Battlefield
invade bf
  | isFinished bf = return bf
  | otherwise     = battle bf >>= invade
  where
    isFinished (Battlefield a d) = a < 2 || d == 0

successProb :: Battlefield -> Rand StdGen Double
successProb b = fractionOf attackersWin <$> simulate noGames (invade b)
  where
    noGames = 100000
    simulate = replicateM
    attackersWin = (== 0) . defenders
    fractionOf p = (/ fromIntegral noGames) . foldl' f 0
      where f = flip $ bool id succ . p
