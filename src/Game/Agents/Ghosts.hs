{-# LANGUAGE NamedFieldPuns #-}

module Game.Agents.Ghosts (
    blinky, pinky, inky, clyde
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


blinky, pinky, inky, clyde :: Agent
blinky = agent (ghost Blinky coordinateZero) ghostSpeed (AIBehaviour blinkyBehaviour)
pinky = agent (ghost Pinky coordinateZero) ghostSpeed (AIBehaviour pinkyBehaviour)
inky = agent (ghost Inky coordinateZero) ghostSpeed (AIBehaviour inkyBehaviour)
clyde = agent (ghost Clyde coordinateZero) ghostSpeed (AIBehaviour clydeBehaviour)

blinkyBehaviour :: Float -> Agent -> World -> Direction
blinkyBehaviour t a@Agent{agentType = Ghost{homePosition}, position, direction, lastTurn} w@World{agents, level}
    | agentPos == lastTurn = direction
    | otherwise = ndirection
      where pacmans = sortClosestAgents position (filterAgentsByType Pacman agents)
            target = case pacmans of
                (Agent{position=p}:as) -> p
                _ -> homePosition
            ndirection = pathFindDumb a w target
            agentPos = coordinateToTile (tiles level) position


pinkyBehaviour :: Float -> Agent -> World -> Direction
pinkyBehaviour t a@Agent{agentType = Ghost{homePosition}, position, direction, lastTurn} w@World{agents, level}
    | agentPos == lastTurn = direction
    | otherwise = ndirection
      where pacmans = sortClosestAgents position (filterAgentsByType Pacman agents)
            target = case pacmans of
                (Agent{position=p, direction=d}:as) -> p + directionToCoordinate d * fromInteger tileSize * 4
                _ -> homePosition
            ndirection = pathFindDumb a w target
            agentPos = coordinateToTile (tiles level) position


inkyBehaviour :: Float -> Agent -> World -> Direction
inkyBehaviour t a@Agent{agentType = Ghost{homePosition}, position, direction, lastTurn} w@World{agents, level}
    | agentPos == lastTurn = direction
    | otherwise = ndirection
      where pacmans = sortClosestAgents position (filterAgentsByType Pacman agents)
            blinkies = sortClosestAgents position (filterAgentsByType (ghost Blinky coordinateZero) agents)
            targetFn (Agent{position=pp, direction=pd}:_) (Agent{position=bp, direction=bd}:_)
                = (pp + directionToCoordinate pd * fromInteger tileSize * 2 - bp) * 2 + position
            targetFn _ _ = homePosition
            target = targetFn pacmans blinkies
            ndirection = pathFindDumb a w target
            agentPos = coordinateToTile (tiles level) position

clydeBehaviour :: Float -> Agent -> World -> Direction
clydeBehaviour t a@Agent{agentType = Ghost{homePosition}, position, direction, lastTurn} w@World{agents, level}
    | agentPos == lastTurn = direction
    | otherwise = ndirection
      where pacmans = sortClosestAgents position (filterAgentsByType Pacman agents)
            targetFn (Agent{position=pp}:_) | distance position pp > fromInteger tileSize * 8 = pp
                                            | otherwise = homePosition
            targetFn _ = homePosition
            target = targetFn pacmans
            ndirection = pathFindDumb a w target
            agentPos = coordinateToTile (tiles level) position

