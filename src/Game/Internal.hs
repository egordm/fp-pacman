{-# LANGUAGE NamedFieldPuns #-}

module Game.Internal (
    World(..),
    Agent(..),
    AgentBehaviour(..),
    SwitchRoom(..),
    SwitchRoomMode(..)
) where

import Engine.Core.Coordinate
import Engine.Graphics.Sprite
import Game.Agents.AgentTypes
import Game.Input
import Game.Level

{- Data structures -}
data World = World {
                 level :: Level,
                 agents :: [Agent]
             } deriving (Show)

data AgentBehaviour = AIBehaviour (Float -> Agent -> World -> Direction) | InputBehaviour InputData

data Agent = Agent {
                 agentType :: AgentType,
                 position :: Coordinate,
                 direction :: Direction,
                 speed :: Float,
                 sprite :: Sprite,
                 behaviour :: AgentBehaviour
             } deriving (Show)

data SwitchRoomMode = ResumeRoom | ReloadRoom deriving(Show)
data SwitchRoom = RoomStay | RoomSwitch String SwitchRoomMode deriving(Show)

{- Classes -}


{- Instances -}
instance Show AgentBehaviour where
    show (AIBehaviour _) = "AI"
    show (InputBehaviour inputData) = show inputData

{- Functions -}
