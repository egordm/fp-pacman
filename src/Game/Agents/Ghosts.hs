module Game.Agents.Ghosts (
    blinky, pinky, inky, clyde, randy
) where
    
import Debug.Trace
import System.Random
import Engine.Core.Base
import Game.Agents.Helpers
import Game.Agents.AgentTypes
import Game.Agents.Agent
import Game.Structure.World
import Game.Level.Level
import Constants
import Resources
import Data.List

    
{- Data structures -}


{- Classes -}


{- Instances -}


{- Functions -}
blinky, pinky, inky, clyde :: Agent
blinky = agent (ghost Blinky blinkyHome False 0) ghostSpeed (AIBehaviour (ghostBehaviourWrapper blinkyBehaviour))
pinky = agent (ghost Pinky pinkyHome True 0) ghostSpeed (AIBehaviour  (ghostBehaviourWrapper pinkyBehaviour))
inky = agent (ghost Inky inkyHome True 30) ghostSpeed (AIBehaviour  (ghostBehaviourWrapper inkyBehaviour))
clyde = agent (ghost Clyde clydeHome True 60) ghostSpeed (AIBehaviour  (ghostBehaviourWrapper clydeBehaviour))
randy = agent (ghost Clyde clydeHome True 0) ghostSpeed (AIBehaviour  (ghostBehaviourWrapper randyBehaviour))

-- | Ghost their home position they run to when they are in scatter mode. See explaination original game.
blinkyHome = Coordinate 9999 9999
pinkyHome = Coordinate (-9999) 9999
inkyHome = Coordinate 9999 (-9999)
clydeHome = Coordinate (-9999) (-9999)

-- | Ghost behaviour. Handles cases which are the same for all ghosts
ghostBehaviourWrapper :: (Float -> Agent -> World -> Direction) -> Float -> Agent -> World -> Direction
ghostBehaviourWrapper behaviour dt a@Agent{agentType=at@Ghost{homePosition}, lastTurn, direction, position} w@World{level}
    | agentPos == lastTurn = direction
    | died at              = pathFindNaive a w (markerCoordinate markerRevivalPoint level)
    | caged at             = pathFindNaive a w (markerCoordinate (agentTypeToMarker at) level)
    | isInScatterMode at   = pathFindNaive a w homePosition
    | inCage               = pathFindNaive a w (markerCoordinate markerDoor level)
    | otherwise            = behaviour dt a w
      where agentPos = coordToTile (tiles level) position
            cageArea = markerCoordinates markerCageCorner (markers level)
            inCage = withinArea position cageArea

-- | Randy walks in a random direction
randyBehaviour :: Float -> Agent -> World -> Direction
randyBehaviour t a@Agent{agentType = at@Ghost{homePosition}, position, direction, lastTurn} w@World{agents, level, rng}
    = case candidateDirs of [] -> DUp
                            _  -> targetDir
      where rndVal = fst $ next rng
            candidateDirs = ghostLegalDirs level a
            targetDir = candidateDirs !! (rndVal `mod` length candidateDirs)

-- | Blinky targets pacman directly
blinkyBehaviour :: Float -> Agent -> World -> Direction
blinkyBehaviour t a@Agent{agentType = at@Ghost{homePosition}, position, direction, lastTurn} w@World{agents, level}
    = pathFindNaive a w target
      where pacmans = sortClosestAgents position (filterAgentsPacman agents)
            target = case pacmans of (Agent{position=p}:as) -> p; _ -> homePosition

-- | Pinky targets position 4 tiles in front of pacman
pinkyBehaviour :: Float -> Agent -> World -> Direction
pinkyBehaviour t a@Agent{agentType = at@Ghost{homePosition}, position, direction, lastTurn} w@World{agents, level}
    = pathFindNaive a w target
      where pacmans = sortClosestAgents position (filterAgentsPacman agents)
            target = case pacmans of
                (Agent{position=p, direction=d}:as) -> p + dirToCoord d * fromInteger tileSize * 4
                _ -> homePosition

-- | Inky targets position (2 tiles in front of pacman - blinkie pos) * 2 + inky pos
inkyBehaviour :: Float -> Agent -> World -> Direction
inkyBehaviour t a@Agent{agentType = at@Ghost{homePosition}, position, direction, lastTurn} w@World{agents, level}
    = pathFindNaive a w target
      where pacmans = sortClosestAgents position (filterAgentsPacman agents)
            blinkies = sortClosestAgents position (filterAgentsByType (ghostEmpty Blinky) agents)
            targetFn (Agent{position=pp, direction=pd}:_) (Agent{position=bp, direction=bd}:_)
                = (pp + dirToCoord pd * fromInteger tileSize * 2 - bp) * 2 + position
            targetFn _ _ = homePosition
            target = targetFn pacmans blinkies

-- | Clyde acts like blinky. But when het is within 8 tiles to pacman, he rushes to his home
clydeBehaviour :: Float -> Agent -> World -> Direction
clydeBehaviour t a@Agent{agentType = at@Ghost{homePosition}, position, direction, lastTurn} w@World{agents, level}
    = pathFindNaive a w target
      where pacmans = sortClosestAgents position (filterAgentsPacman agents)
            targetFn (Agent{position=pp}:_) | coordDist position pp > fromInteger tileSize * 8 = pp
                                            | otherwise = homePosition
            targetFn _ = homePosition
            target = targetFn pacmans
