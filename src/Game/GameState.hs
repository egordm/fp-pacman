{-# LANGUAGE NamedFieldPuns #-}

module Game.GameState (
    GameState(..),
    Updateable(..),
    Renderable(..),
    Inputable(..),
    makeState
) where

import Engine.Core.Classes
import Game.World
import Game.Input
import Game.Agent
import Debug.Trace
import Game.SwitchRoom

{- Data structures -}
data GameState = GameState {
                     t :: Float,
                     world :: World,
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

makeState level bois = GameState 0 world RoomStay
                       where baseWorld = World level []
                             world = foldr addAgent baseWorld bois
