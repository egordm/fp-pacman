module Game.Agents.Ghosts (
    blinky, pinky, inky, clyde
) where
    
import Debug.Trace
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
blinky = agent (ghost Blinky blinkyHome) ghostSpeed (AIBehaviour (ghostBehaviourWrapper blinkyBehaviour))
pinky = agent (ghost Pinky pinkyHome) ghostSpeed (AIBehaviour  (ghostBehaviourWrapper pinkyBehaviour))
inky = agent (ghost Inky inkyHome) ghostSpeed (AIBehaviour  (ghostBehaviourWrapper inkyBehaviour))
clyde = agent (ghost Clyde clydeHome) ghostSpeed (AIBehaviour  (ghostBehaviourWrapper clydeBehaviour))

blinkyHome = Coordinate 9999 9999
pinkyHome = Coordinate (-9999) 9999
inkyHome = Coordinate 9999 (-9999)
clydeHome = Coordinate (-9999) (-9999)

-- | Ghost behaviour. Handles cases which are the same for all ghosts
ghostBehaviourWrapper :: (Float -> Agent -> World -> Direction) -> Float -> Agent -> World -> Direction
ghostBehaviourWrapper behaviour dt a@Agent{agentType=at@Ghost{homePosition}, lastTurn, direction, position} w@World{level}
    | agentPos == lastTurn = direction
    | died at              = pathFindDumb a w (markerCoordinate (Marker 'R') level)
    | isInScatterMode at   = pathFindDumb a w homePosition
    | otherwise            = behaviour dt a w
      where agentPos = coordToTile (tiles level) position

-- | Blinky targets pacman directly
blinkyBehaviour :: Float -> Agent -> World -> Direction
blinkyBehaviour t a@Agent{agentType = at@Ghost{homePosition}, position, direction, lastTurn} w@World{agents, level}
    = pathFindDumb a w target
      where pacmans = sortClosestAgents position (filterAgentsPacman agents)
            target = case pacmans of (Agent{position=p}:as) -> p; _ -> homePosition

-- | Pinky targets position 4 tiles in front of pacman
pinkyBehaviour :: Float -> Agent -> World -> Direction
pinkyBehaviour t a@Agent{agentType = at@Ghost{homePosition}, position, direction, lastTurn} w@World{agents, level}
    = pathFindDumb a w target
      where pacmans = sortClosestAgents position (filterAgentsPacman agents)
            target = case pacmans of
                (Agent{position=p, direction=d}:as) -> p + dirToCoord d * fromInteger tileSize * 4
                _ -> homePosition

-- | Inky targets position (2 tiles in front of pacman - blinkie pos) * 2 + inky pos
inkyBehaviour :: Float -> Agent -> World -> Direction
inkyBehaviour t a@Agent{agentType = at@Ghost{homePosition}, position, direction, lastTurn} w@World{agents, level}
    = pathFindDumb a w target
      where pacmans = sortClosestAgents position (filterAgentsPacman agents)
            blinkies = sortClosestAgents position (filterAgentsByType (ghost Blinky coordZ) agents)
            targetFn (Agent{position=pp, direction=pd}:_) (Agent{position=bp, direction=bd}:_)
                = (pp + dirToCoord pd * fromInteger tileSize * 2 - bp) * 2 + position
            targetFn _ _ = homePosition
            target = targetFn pacmans blinkies

-- | Clyde acts like blinky. But when het is within 8 tiles to pacman, he rushes to his home
clydeBehaviour :: Float -> Agent -> World -> Direction
clydeBehaviour t a@Agent{agentType = at@Ghost{homePosition}, position, direction, lastTurn} w@World{agents, level}
    = pathFindDumb a w target
      where pacmans = sortClosestAgents position (filterAgentsPacman agents)
            targetFn (Agent{position=pp}:_) | coordDist position pp > fromInteger tileSize * 8 = pp
                                            | otherwise = homePosition
            targetFn _ = homePosition
            target = targetFn pacmans

