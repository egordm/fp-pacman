module Game.Agents.Helpers (
    sortClosestAgents,
    filterAgentsByType,
    filterAgentsGhost,
    filterAgentsPacman,
    pathFindDumb
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
    = case rankeddirs of (r:_) -> r
                         _     -> direction
      where agentPos = coordToTile (tiles level) position
            candidatedirs = ghostMoveDirectionCandidates level a
            directionTargetcoordDist d = coordDist target (position + dirToCoord d)
            rankeddirs = sortBy (\a b -> compare (directionTargetcoordDist a) (directionTargetcoordDist b)) candidatedirs


ghostMoveDirectionCandidates :: Level -> Agent -> [Direction]
ghostMoveDirectionCandidates l@Level{tiles, markers} Agent{position, direction}
  = freedirs
    where agentPos = coordToTile tiles position
          legaldirs = filter (\d -> d /= DNone && d /= dirOpposite direction) dirs
          freedirs = filter (\d -> isGhostWalkablePos l (agentPos + dirToPos d)) legaldirs

isGhostWalkablePos :: Level -> Pos -> Bool
isGhostWalkablePos Level{tiles} pos = case tiles ! pos of TileWall _ -> False
                                                          TileMarker (Marker '9') -> False
                                                          _ -> True
