module Game.GameState (
    GameState(..),
    Updateable(..),
    Renderable(..)
) where
    
import Game.World
import Engine.Core.Classes

{- Data structures -}
data GameState = GameState {
                     t :: Float,
                     world :: World
                 } deriving (Show)

{- Classes -}


{- Instances -}
instance Updateable GameState where
    update dt t (GameState pt world) = GameState t (update dt t world)

instance Renderable GameState where
    render (GameState t world) = render world

{- Functions -}

