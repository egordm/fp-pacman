module Game.Internal (
    World(..),
    Agent(..)
) where

import Engine.Core.Coordinate
import Engine.Graphics.Sprite
import Game.Agents.AgentTypes
import Game.Input

{- Data structures -}
data World = World {
                 test :: Sprite, -- TODO: temporary. while we have nothing
                 agents :: [Agent]
             } deriving (Show)

data AgentBehaviour = AIBehaviour (Float -> Agent -> World -> Direction) | InputBehaviour InputData

data Agent = Agent {
                 agentType :: AgentType,
                 position :: Coordinate,
                 direction :: Direction,
                 sprite :: Sprite,
                 behaviour :: AgentBehaviour
             }

{- Classes -}


{- Instances -}
instance Show Agent where
    show (Agent agentType position direction sprite _) = show agentType ++ show position ++ show direction

{- Functions -}

