{-# LANGUAGE NamedFieldPuns #-}
module Game.Agents.Helpers (
    sortClosestAgents,
    filterAgentsByType,
    filterAgentsGhost,
    filterAgentsPacman,
    pathFindNaive,
    pathFindDumb,
    agentSetScatterTicks,
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
sortClosestAgents :: Coordinate -> [Agent] -> [Agent]
sortClosestAgents c = sortBy (\Agent{position=p1} Agent{position=p2} -> compare (coordDist p1 c) (coordDist p2 c))

filterAgentsByType :: AgentType -> [Agent] -> [Agent]
filterAgentsByType atype = filter (\Agent{agentType} -> atype == agentType)

filterAgentsPacman :: [Agent] -> [Agent]
filterAgentsPacman = filterAgentsByType Pacman{died=False}

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

pathFindNaive :: Agent -> World -> Coordinate -> Direction
pathFindNaive a@Agent{position, direction} World{level} target
    = case rankedCandidates of
        (r:_) -> r
        _     -> direction
      where agentPos = coordToTile (tiles level) position
            candidates = ghostLegalDirs level a
            rankedCandidates = rankDirs position target candidates

ghostLegalDirs :: Level -> Agent -> [Direction]
ghostLegalDirs l a@Agent{direction} = filter (\d -> d /= dirOpposite direction) (ghostWalkableDirs l a)

ghostWalkableDirs :: Level -> Agent -> [Direction]
ghostWalkableDirs l@Level{tiles, markers} a@Agent{position, direction}
  = filter (\d -> d /= DNone && isGhostWalkablePos l a (agentPos + dirToPos d)) dirs
    where agentPos = coordToTile tiles position

rankDirs :: Coordinate -> Coordinate -> [Direction] -> [Direction]
rankDirs position target directions = sortBy (\a b -> compare (distToTarget a) (distToTarget b)) directions
                                      where distToTarget d = coordDist target (position + dirToCoord d)

isGhostWalkablePos :: Level -> Agent -> Pos -> Bool
isGhostWalkablePos Level{tiles, markers} Agent{agentType, position} pos = case tiles ! pos of
    TileWall _ -> False
    TileMarker (Marker '9') -> False
    TileMarker (Marker '_') -> inCage || died agentType
    _ -> True
    where cageArea = markerCoordinates markerCageCorner markers
          inCage = withinArea position cageArea

agentSetScatterTicks :: Float -> Agent -> Agent
agentSetScatterTicks _ a@Agent{agentType=Pacman{}} = a
agentSetScatterTicks t a@Agent{agentType=at@Ghost{}} = a{agentType=at{scatterTicks=t}}

withinArea :: Coordinate -> [Coordinate] -> Bool
withinArea (Coordinate x y) ((Coordinate x1 y1):(Coordinate x2 y2):_)
    = x <= max x1 x2 && x >= min x1 x2 && y <= max y1 y2 && y >= min y1 y2
withinArea _ _ = False