{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HW12.Risk where

import StrippedPrelude
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
dieN n = sortDesc <$> replicateM n die
  where sortDesc = sortBy (flip compare)

limitAttackers :: Army -> Army
limitAttackers = min 3 . pred

limitDefenders :: Army -> Army
limitDefenders = min 2

resolveThrow :: Battlefield -> (DieValue, DieValue) -> Battlefield
resolveThrow bf@(Battlefield as ds) (af, df)
  | af > df   = bf { defenders = pred ds }
  | otherwise = bf { attackers = pred as }

-- Ex 2
battle :: Battlefield -> Rand StdGen Battlefield
battle bf = do
  attack <- dieN (limitAttackers $ attackers bf)
  defend <- dieN (limitDefenders $ defenders bf)
  return $ foldl' resolveThrow bf $ zip attack defend

-- Ex 3
invade :: Battlefield -> Rand StdGen Battlefield
invade b = (bool invade return =<< isFinished) =<< battle b
  where
    isFinished (Battlefield a d) = a < 2 || d == 0

successProb :: Battlefield -> Rand StdGen Double
successProb b = (/ noGames) <$> foldM f 0 [1..noGames]
  where
    noGames = 100
    f n _ = (n +) . bool 0 1 . attackersWin <$> invade b
    attackersWin = (== 0) . defenders
