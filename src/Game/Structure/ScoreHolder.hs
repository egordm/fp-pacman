module Game.Structure.ScoreHolder (
    ScoreHolder(..),
    incrementScore,
    decrementLife,
    scoreholder
) where

import Constants
import Engine.Base
import Game.UI.Base

{- Data structures -}
data ScoreHolder = ScoreHolder {
                      score :: Int, -- TODO: do we need to store this per pacman mb?
                      lives :: Int
                   } deriving (Show, Ord, Eq)

{- Classes -}


{- Instances -}
instance Drawable ScoreHolder where
    draw ScoreHolder{score = sc, lives = lv} = draw $ FontString msg coor
        where
            msg = "score - " ++ show sc ++ "    lives - " ++ show lv
            coor = (Coordinate (fromIntegral((-width)`div`2)) (fromIntegral((-height)`div`2)) + 20)

{- Functions -}
scoreholder :: ScoreHolder
scoreholder = ScoreHolder 0 liveCount

incrementScore :: ScoreHolder -> Int -> ScoreHolder
incrementScore s@ScoreHolder{score=pscore} amount = s{score=pscore + amount}

decrementLife :: ScoreHolder -> ScoreHolder
decrementLife s = s{lives=lives s - 1}
