{-# LANGUAGE NamedFieldPuns #-}

module Game.Agent (
    Agent(..),
    Drawable(..),
    AgentBehaviour(..),
    updateAgent,
    updateAgentDirection
) where
    
import Debug.Trace
import Engine.Core.Classes
import Engine.Core.Coordinate
import Engine.Graphics.Rendering
import Engine.Graphics.Sprite
import Game.Internal(Agent(..), AgentBehaviour(..), World(..))
import Game.Input
import Game.Agents.AgentTypes

{- Data structures -}

{- Classes -}

{- Instances -}
instance Inputable Agent where
    input event a@Agent{behaviour=InputBehaviour inputData} = agent -- trace (show agent) agent
                                                              where agent = a{behaviour=InputBehaviour (input event inputData)}
    input _ a = a

instance Drawable Agent where
    draw a@Agent{position, sprite} = [DrawInstruction position sprite]

{- Functions -}
updateAgent :: Float -> Float -> World -> Agent -> Agent
updateAgent dt t world a@Agent{sprite, agentType, position, direction, speed, behaviour}
     = a{sprite=nsprite, direction=ndirection, position=nposition}
       where
          ndirection = updateAgentDirection t world a behaviour
          nsprite = update dt t (updateAgentSprite sprite (agentTypeToSprite ndirection agentType))
          nposition = position + (directionToCoordinate direction) * (fromFloat (speed * dt))


updateAgentDirection :: Float -> World -> Agent -> AgentBehaviour -> Direction
updateAgentDirection t world agent (AIBehaviour aiFn) = aiFn t agent world
updateAgentDirection _ _ a@Agent{direction} (InputBehaviour (InputData _ newDirection))
    = case newDirection of DNone -> direction
                           _     -> newDirection

updateAgentSprite :: Sprite -> Sprite -> Sprite
updateAgentSprite old new | old == new = old
                          | otherwise = new
