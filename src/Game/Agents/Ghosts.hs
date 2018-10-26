{-# LANGUAGE NamedFieldPuns #-}

module Game.Agents.Ghosts (
    blinky, pinky
) where
    
import Debug.Trace
import Engine.Core.Coordinate
import Game.Agents.Helpers
import Game.Agents.AgentTypes
import Game.Agent
import Game.World
import Game.Level.Level
import Constants
import Resources
import Data.List

    
{- Data structures -}


{- Classes -}


{- Instances -}


{- Functions -}


blinky, pinky :: Agent
blinky = agent (ghost Blinky coordinateZero) ghostSpeed (AIBehaviour blinkyBehaviour)
pinky = agent (ghost Pinky coordinateZero) ghostSpeed (AIBehaviour pinkyBehaviour)

blinkyBehaviour :: Float -> Agent -> World -> Direction
blinkyBehaviour t a@Agent{position, direction, lastTurn} w@World{agents, level}
    | agentPos == lastTurn = direction
    | otherwise = ndirection
      where pacmans = sortClosestAgents position (filterAgentsByType Pacman agents)
            target = case pacmans of (Agent{position=p}:as) -> p; _ -> coordinateZero
            ndirection = pathFindDumb a w target
            agentPos = coordinateToTile (tiles level) position


pinkyBehaviour :: Float -> Agent -> World -> Direction
pinkyBehaviour t a@Agent{position, direction, lastTurn} w@World{agents, level}
    | agentPos == lastTurn = direction
    | otherwise = ndirection
      where pacmans = sortClosestAgents position (filterAgentsByType Pacman agents)
            target = case pacmans of (Agent{position=p, direction=d}:as) -> p + directionToCoordinate d * 4; _ -> coordinateZero
            ndirection = pathFindDumb a w target
            agentPos = coordinateToTile (tiles level) position
