{-# LANGUAGE NamedFieldPuns #-}
module Game.Structure.World (
    World(..),
    Updateable(..),
    Renderable(..),
    Resetable(..),
    addAgent,
    addAgents
) where

import Debug.Trace
import Data.List
import Engine.Graphics.Base
import Engine.Core.Base
import Game.Structure.Internal(World(..))
import Game.Input.Base
import Game.Agents.Agent
import Game.Agents.AgentTypes
import Game.Level.Base

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

instance Resetable World where
    reset w@World{agents} = addAgents nagents w{agents=[]}
                            where nagents = map reset agents

{- Functions -}
-- | Adds agent to world. Also sets position based on the markers in the level
addAgent :: Agent -> World -> World
addAgent a@Agent{agentType} w@World{level, agents}
    = w{agents=nagents}
      where nagents = a{position=coord}:agents
            coord = markerCoordinate (agentTypeToMarker agentType) level

addAgents :: [Agent] -> World -> World
addAgents agents world = foldr addAgent world agents