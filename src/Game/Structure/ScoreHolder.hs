module Game.Structure.ScoreHolder (
    ScoreHolder(..),
    incrementScore,
    decrementLife,
    scoreholder
) where

import Constants

{- Data structures -}
data ScoreHolder = ScoreHolder {
                      score :: Int, -- TODO: do we need to store this per pacman mb?
                      lives :: Int
                   } deriving (Show, Ord, Eq)

{- Classes -}


{- Instances -}


{- Functions -}
scoreholder :: ScoreHolder
scoreholder = ScoreHolder 0 liveCount

incrementScore :: ScoreHolder -> Int -> ScoreHolder
incrementScore s@ScoreHolder{score=pscore} amount = s{score=pscore + amount}

decrementLife :: ScoreHolder -> ScoreHolder
decrementLife s = s{lives=lives s - 1}
