module Game.Structure.ScoreHolder (
    ScoreHolder(..),
    incrementScore,
    scoreholder
) where
    

{- Data structures -}
data ScoreHolder = ScoreHolder {
                      score :: Int -- TODO: do we need to store this per pacman mb?
                   } deriving (Show, Ord, Eq)

{- Classes -}


{- Instances -}


{- Functions -}
scoreholder :: ScoreHolder
scoreholder = ScoreHolder 0

incrementScore :: ScoreHolder -> Int -> ScoreHolder
incrementScore s@ScoreHolder{score=pscore} amount = s{score=pscore + amount}

