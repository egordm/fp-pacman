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
blinky = agent (ghost Blinky coordZ) ghostSpeed (AIBehaviour blinkyBehaviour)
pinky = agent (ghost Pinky coordZ) ghostSpeed (AIBehaviour pinkyBehaviour)
inky = agent (ghost Inky coordZ) ghostSpeed (AIBehaviour inkyBehaviour)
clyde = agent (ghost Clyde coordZ) ghostSpeed (AIBehaviour clydeBehaviour)

blinkyBehaviour :: Float -> Agent -> World -> Direction
blinkyBehaviour t a@Agent{agentType = Ghost{homePosition}, position, direction, lastTurn} w@World{agents, level}
    | agentPos == lastTurn = direction
    | otherwise = ndirection
      where pacmans = sortClosestAgents position (filterAgentsByType Pacman agents)
            target = case pacmans of
                (Agent{position=p}:as) -> p
                _ -> homePosition
            ndirection = pathFindDumb a w target
            agentPos = coordToTile (tiles level) position


pinkyBehaviour :: Float -> Agent -> World -> Direction
pinkyBehaviour t a@Agent{agentType = Ghost{homePosition}, position, direction, lastTurn} w@World{agents, level}
    | agentPos == lastTurn = direction
    | otherwise = ndirection
      where pacmans = sortClosestAgents position (filterAgentsByType Pacman agents)
            target = case pacmans of
                (Agent{position=p, direction=d}:as) -> p + dirToCoord d * fromInteger tileSize * 4
                _ -> homePosition
            ndirection = pathFindDumb a w target
            agentPos = coordToTile (tiles level) position


inkyBehaviour :: Float -> Agent -> World -> Direction
inkyBehaviour t a@Agent{agentType = Ghost{homePosition}, position, direction, lastTurn} w@World{agents, level}
    | agentPos == lastTurn = direction
    | otherwise = ndirection
      where pacmans = sortClosestAgents position (filterAgentsByType Pacman agents)
            blinkies = sortClosestAgents position (filterAgentsByType (ghost Blinky coordZ) agents)
            targetFn (Agent{position=pp, direction=pd}:_) (Agent{position=bp, direction=bd}:_)
                = (pp + dirToCoord pd * fromInteger tileSize * 2 - bp) * 2 + position
            targetFn _ _ = homePosition
            target = targetFn pacmans blinkies
            ndirection = pathFindDumb a w target
            agentPos = coordToTile (tiles level) position

clydeBehaviour :: Float -> Agent -> World -> Direction
clydeBehaviour t a@Agent{agentType = Ghost{homePosition}, position, direction, lastTurn} w@World{agents, level}
    | agentPos == lastTurn = direction
    | otherwise = ndirection
      where pacmans = sortClosestAgents position (filterAgentsByType Pacman agents)
            targetFn (Agent{position=pp}:_) | coordDist position pp > fromInteger tileSize * 8 = pp
                                            | otherwise = homePosition
            targetFn _ = homePosition
            target = targetFn pacmans
            ndirection = pathFindDumb a w target
            agentPos = coordToTile (tiles level) position

