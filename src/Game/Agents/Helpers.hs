module Game.Agents.Helpers (
    sortClosestAgents,
    filterAgentsByType,
    filterAgentsGhost,
    filterAgentsPacman,
    pathFindDumb,
    agentSetScatterTicks
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

pathFindDumb :: Agent -> World -> Coordinate -> Direction
pathFindDumb a@Agent{position, direction} World{level} target
    = case rankedCandidates of
        (r:_) -> r
        _     -> direction
      where agentPos = coordToTile (tiles level) position
            candidates = ghostLegalDirs level a
            rankedCandidates = rankDirs position target candidates

ghostLegalDirs :: Level -> Agent -> [Direction]
ghostLegalDirs l a@Agent{direction} = filter (\d -> d /= dirOpposite direction) (ghostWalkableDirs l a)

ghostWalkableDirs :: Level -> Agent -> [Direction]
ghostWalkableDirs l@Level{tiles, markers} Agent{position, direction}
  = filter (\d -> d /= DNone && isGhostWalkablePos l (agentPos + dirToPos d)) dirs
    where agentPos = coordToTile tiles position

rankDirs :: Coordinate -> Coordinate -> [Direction] -> [Direction]
rankDirs position target directions = sortBy (\a b -> compare (distToTarget a) (distToTarget b)) directions
                                      where distToTarget d = coordDist target (position + dirToCoord d)

isGhostWalkablePos :: Level -> Pos -> Bool
isGhostWalkablePos Level{tiles} pos = case tiles ! pos of TileWall _ -> False
                                                          TileMarker (Marker '9') -> False
                                                          _ -> True

agentSetScatterTicks :: Float -> Agent -> Agent
agentSetScatterTicks _ a@Agent{agentType=Pacman{}} = a
agentSetScatterTicks t a@Agent{agentType=at@Ghost{}} = a{agentType=at{scatterTicks=t}}

