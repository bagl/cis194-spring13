-- | Main entry point to the application.
module HW02.Hanoi where

import Control.Applicative ((<$>))
import Control.Monad (foldM)

data Peg   = A | B | C | D deriving (Show)
type Move  = (Peg, Peg)
type Discs = [Int]
type HanoiState = (Discs, Discs, Discs, Discs)
type Hanoi3 = Int -> Peg -> Peg -> Peg -> [Move]
type Hanoi4 = Int -> Peg -> Peg -> Peg -> Peg -> [Move]

-- | The main entry point.
main :: IO ()
main = case hanoiStates hanoi4 15 of
    Left  err    -> print err
    Right states -> putStrLn $ unlines $ map show states

-- | Solver for Hanoi game with 3 pegs
hanoi3 :: Hanoi3
hanoi3 0 _ _ _ = []
hanoi3 n a b c = hanoi3 (n-1) a c b ++ [(a, b)] ++ hanoi3 (n-1) c b a

-- | Solver for Hanoi game with 4 pegs
hanoi4 :: Hanoi4
hanoi4 0 _ _ _ _ = []
hanoi4 n a d b c = hanoi4 m a b c d ++ hanoi3 (n-m) a d c ++ hanoi4 m b d a c
    where m = n - floor (sqrt (fromIntegral $ 2 * n) + 1/2 :: Float)

-- | Returns a list of game states calculated from moves returned by Hanoi game solver
hanoiStates :: Hanoi4                     -- ^ Hanoi game solver for 4 pegs
            -> Int                        -- ^ Number of discs to play the game with
            -> Either String [HanoiState] -- ^ Either error string or a list of game states
hanoiStates solver noDiscs = reverse <$> foldM prependNextState [initialState] moves
    where
        prependNextState :: [HanoiState] -> Move -> Either String [HanoiState]
        prependNextState states@(s:_) move = (: states) <$> makeMove s move
        prependNextState []           _    = Left "Empty list of states given to 'prependNextState'"

        initialState :: HanoiState
        initialState = ([1..noDiscs], [], [], [])

        moves :: [Move]
        moves = solver noDiscs A D B C

-- | Given the initial game state, 'makeMove' makes the move and returns either the resulting state or an error string
makeMove :: HanoiState               -- ^ Initial game state
         -> Move                     -- ^ Move to be done
         -> Either String HanoiState -- ^ Either error message if the move cannot be done or an updated game state
makeMove state (fromPeg, toPeg) = moveTopDisc (getDiscs fromPeg state) (getDiscs toPeg state)
  where
    moveTopDisc []     _        = Left  $ "Cannot draw disc from empty peg " ++ show fromPeg ++ " in state: " ++ show state
    moveTopDisc (f:fs) []       = Right $ putDiscs toPeg [f]    $ putDiscs fromPeg fs state
    moveTopDisc (f:fs) ts@(t:_)
                | f < t         = Right $ putDiscs toPeg (f:ts) $ putDiscs fromPeg fs state
                | otherwise     = Left  $ "Cannot move disc " ++ show f ++ " onto disc " ++ show t ++ " in state: " ++ show state

-- ===================================================
-- Substitute for `Data.Map Peg Discs`

-- | Returns a list of discs on a given peg
getDiscs :: Peg        -- ^ Peg to get discs from
         -> HanoiState -- ^ State of the game
         -> Discs      -- ^ List of discs on a given peg
getDiscs A (a, _, _, _) = a
getDiscs B (_, b, _, _) = b
getDiscs C (_, _, c, _) = c
getDiscs D (_, _, _, d) = d

-- | Retruns updated state after inserting a new list of discs to a given peg
putDiscs :: Peg        -- ^ Peg to update
         -> Discs      -- ^ New list of discs to be put on the given peg
         -> HanoiState -- ^ Initial state of the game
         -> HanoiState -- ^ Updated game state
putDiscs A a (_, b, c, d) = (a, b, c, d)
putDiscs B b (a, _, c, d) = (a, b, c, d)
putDiscs C c (a, b, _, d) = (a, b, c, d)
putDiscs D d (a, b, c, _) = (a, b, c, d)
