{-# LANGUAGE NamedFieldPuns #-}

module Game.World (
    World(..),
    Updateable(..),
    Renderable(..),
    addAgent
) where

import Data.List
import Engine.Graphics.Sprite
import Engine.Graphics.Rendering
import Engine.Core.Classes
import Engine.Core.Coordinate
import Game.Internal(World(..))
import Game.Input
import Game.Agents.AgentTypes
import Game.Agent
import Game.Level.Level
import Debug.Trace

{- Data structures -}

{- Classes -}

{- Instances -}
instance Updateable World where
    update dt t w@World{agents} = w{agents=nagents}
                                  where nagents = map (updateAgent dt t w) agents

instance Renderable World where
    render World{agents, level} = renderInstructions (levelDrawings ++ agentDrawings)
                                  where agentDrawings = concatMap draw agents
                                        levelDrawings = draw level

instance Inputable World where
    input event w@World{agents} = w{agents = nagents}
                                  where nagents = map (input event) agents

{- Functions -}
addAgent :: Agent -> World -> World
addAgent a@Agent{agentType} w@World{level, agents} = w{agents=nagents}
                                             where nagents = a{position=coord}:agents
                                                   coord = markerCoordinate (agentTypeToMarker agentType) level
