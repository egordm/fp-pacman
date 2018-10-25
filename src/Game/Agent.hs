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
import Game.Level.Level
import Constants

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
     = a{sprite=nsprite, direction=ndirection, position=sposition}
       where
          ndirection = updateAgentDirection t world a behaviour
          nsprite = update dt t (updateAgentSprite sprite (agentTypeToSprite ndirection agentType))
          sposition = updateAgentPosition dt world a


updateAgentDirection :: Float -> World -> Agent -> AgentBehaviour -> Direction
updateAgentDirection t world agent (AIBehaviour aiFn) = aiFn t agent world
updateAgentDirection _ _ a@Agent{direction} (InputBehaviour (InputData _ newDirection))
    = case newDirection of DNone -> direction
                           _     -> newDirection

updateAgentSprite :: Sprite -> Sprite -> Sprite
updateAgentSprite old new | old == new = old
                          | otherwise = new

updateAgentPosition :: Float -> World -> Agent -> Coordinate
updateAgentPosition dt World{level} a@Agent{position, direction, speed}
    = position + delta
      where deltaTarget = (directionToCoordinate direction) * (fromFloat (speed * dt))
            deltaTile = tileToCoordinate (tiles level) (coordinateToTile (tiles level) position) - position
            orthDir = orthagonalDirection direction
            canTurn = distance tilePos position < epsilon
            delta = coordinateComponent direction deltaTarget + coordinateComponent orthDir tilePos

