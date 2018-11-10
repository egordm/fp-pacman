module Game.Agents.Helpers (
    sortClosestAgents,
    filterAgentsByType,
    filterAgentsGhost,
    filterAgentsPacman,
    pathFindNaive,
    pathFindDumb,
    agentSetScatterTicks,
    ghostLegalDirs,
    withinArea
) where

import Engine.Core.Base
import Game.Agents.AgentTypes
import Game.Agents.Agent
import Game.Structure.World
import Game.Level.Level
import Data.List
import Debug.Trace

    
{- Data structures -}


{- Classes -}


{- Instances -}


{- Functions -}
-- | SOrt agents by closest to position
sortClosestAgents :: Coordinate -> [Agent] -> [Agent]
sortClosestAgents c = sortBy (\Agent{position=p1} Agent{position=p2} -> compare (coordDist p1 c) (coordDist p2 c))

-- | Filter agents by type
filterAgentsByType :: AgentType -> [Agent] -> [Agent]
filterAgentsByType atype = filter (\Agent{agentType} -> atype == agentType)

-- | Get all pacmans
filterAgentsPacman :: [Agent] -> [Agent]
filterAgentsPacman = filterAgentsByType Pacman{died=False}

-- | Get all ghosts
filterAgentsGhost :: [Agent] -> [Agent]
filterAgentsGhost [] = []
filterAgentsGhost (a@Agent{agentType}:as) = case agentType of Ghost{} -> a:(filterAgentsGhost as)
                                                              _       -> filterAgentsGhost as

-- | Dont use this, unless you want your pacman to come stuck
pathFindDumb :: Agent -> World -> Coordinate -> Direction
pathFindDumb a@Agent{position, direction} World{level} target
    = case rankedCandidates of
        (r:_) -> r
        _     -> direction
      where agentPos = coordToTile (tiles level) position
            candidates = ghostWalkableDirs level a
            rankedCandidates = rankDirs position target candidates

-- | Get legal walkable direction which brings agent closer to its target
pathFindNaive :: Agent -> World -> Coordinate -> Direction
pathFindNaive a@Agent{position, direction} World{level} target
    = case rankedCandidates of
        (r:_) -> r
        _     -> direction
      where agentPos = coordToTile (tiles level) position
            candidates = ghostLegalDirs level a
            rankedCandidates = rankDirs position target candidates

-- | Get legal directions for ghosts. Ghosts cant do 180 degree turns
ghostLegalDirs :: Level -> Agent -> [Direction]
ghostLegalDirs l a@Agent{direction} = filter (\d -> d /= dirOpposite direction) (ghostWalkableDirs l a)

-- | Get ghost walkable directions. Chekc if direction is free and is not none. Since ghost cant stand still
ghostWalkableDirs :: Level -> Agent -> [Direction]
ghostWalkableDirs l@Level{tiles, markers} a@Agent{position, direction}
  = filter (\d -> d /= DNone && isGhostWalkablePos l a (agentPos + dirToPos d)) dirs
    where agentPos = coordToTile tiles position

-- | Rank/order directions by closest to target first
rankDirs :: Coordinate -> Coordinate -> [Direction] -> [Direction]
rankDirs position target directions = sortBy (\a b -> compare (distToTarget a) (distToTarget b)) directions
                                      where distToTarget d = coordDist target (position + dirToCoord d)

-- | Check if position is walkable. Checks if direction is obstructed by a wall or by a door if ghost is outside of the cage and not dead
-- | so it wont turn back, but can return if its dead
isGhostWalkablePos :: Level -> Agent -> Pos -> Bool
isGhostWalkablePos Level{tiles, markers} Agent{agentType, position} pos = case tiles ! pos of
    TileWall _ -> False
    TileMarker (Marker '9') -> False
    TileMarker (Marker '_') -> inCage || died agentType
    _ -> True
    where cageArea = markerCoordinates markerCageCorner markers
          inCage = withinArea position cageArea

-- | Sets agent scatter ticks. Only does so on ghosts. Just a helper
agentSetScatterTicks :: Float -> Agent -> Agent
agentSetScatterTicks _ a@Agent{agentType=Pacman{}} = a
agentSetScatterTicks t a@Agent{agentType=at@Ghost{}} = a{agentType=at{scatterTicks=t}}

-- | Checks whether a coordinate is within a rectangular area. It is unknown which coordinate is which corner
withinArea :: Coordinate -> [Coordinate] -> Bool
withinArea (Coordinate x y) ((Coordinate x1 y1):(Coordinate x2 y2):_)
    = x <= max x1 x2 && x >= min x1 x2 && y <= max y1 y2 && y >= min y1 y2
withinArea _ _ = False