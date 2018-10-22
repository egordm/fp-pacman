{-# LANGUAGE NamedFieldPuns #-}

module Game.GameState (
    GameState(..),
    Updateable(..),
    Renderable(..),
    Inputable(..)
) where
    
import Engine.Core.Classes
import Game.World
import Game.Input
import Game.Agent
import Debug.Trace

{- Data structures -}
data GameState = GameState {
                     t :: Float,
                     world :: World
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

