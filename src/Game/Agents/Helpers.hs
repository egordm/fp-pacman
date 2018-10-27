{-# LANGUAGE NamedFieldPuns #-}

module Game.Agents.Helpers (
    sortClosestAgents,
    filterAgentsByType,
    pathFindDumb
) where

import Engine.Core.Coordinate
import Game.Agents.AgentTypes
import Game.Agent
import Game.World
import Game.Level.Level
import Data.List
import Debug.Trace

    
{- Data structures -}


{- Classes -}


{- Instances -}


{- Functions -}
sortClosestAgents :: Coordinate -> [Agent] -> [Agent]
sortClosestAgents c = sortBy (\Agent{position=p1} Agent{position=p2} -> compare (distance p1 c) (distance p2 c))


filterAgentsByType :: AgentType -> [Agent] -> [Agent]
filterAgentsByType atype = filter (\Agent{agentType} -> atype == agentType)


pathFindDumb :: Agent -> World -> Coordinate -> Direction
pathFindDumb a@Agent{position, direction} World{level} target
    = case rankedDirections of (r:_) -> r
                               _     -> direction
      where agentPos = coordinateToTile (tiles level) position
            candidateDirections = ghostMoveDirectionCandidates level a
            directionTargetDistance d = distance target (position + directionToCoordinate d)
            rankedDirections = sortBy (\a b -> compare (directionTargetDistance a) (directionTargetDistance b)) candidateDirections


ghostMoveDirectionCandidates :: Level -> Agent -> [Direction]
ghostMoveDirectionCandidates l@Level{tiles, markers} Agent{position, direction}
  = freeDirections
    where agentPos = coordinateToTile tiles position
          legalDirections = filter (\d -> d /= DNone && d /= oppositeDirection direction) directions
          freeDirections = filter (\d -> isGhostWalkablePos l (agentPos + directionToPos d)) legalDirections

isGhostWalkablePos :: Level -> Pos -> Bool
isGhostWalkablePos Level{tiles} pos = case tiles ! pos of TileWall _ -> False
                                                          TileMarker (Marker '9') -> False
                                                          _ -> True
