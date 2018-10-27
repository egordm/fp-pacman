module Game.Structure.GameState (
    GameState(..),
    Updateable(..),
    Renderable(..),
    Inputable(..),
    makeState
) where

import Debug.Trace
import Engine.Core.Base
import Game.Structure.World
import Game.Structure.ScoreHolder
import Game.Input.Base
import Game.Agents.Base
import Game.Context.SwitchRoom

{- Data structures -}
data GameState = GameState {
                     t :: Float,
                     world :: World,
                     scoreInfo :: ScoreHolder,
                     switch :: SwitchRoom
                 } deriving (Show)

{- Classes -}


{- Instances -}
instance Updateable GameState where
    update dt nt s@GameState{t=pt, world=pworld} = s{t=nt, world=update dt nt pworld}

instance Renderable GameState where
    render GameState{world=world} = render world

instance Inputable GameState where
    input event s@GameState{world=pworld} = s{world=input event pworld}

{- Functions -}

makeState level bois = GameState 0 world scoreholder RoomStay
                       where baseWorld = World level []
                             world = foldr addAgent baseWorld bois
