{-# LANGUAGE NamedFieldPuns #-}

module Game.Agent (
    Agent(..),
    Drawable(..),
    AgentBehaviour(..),
    updateAgent,
    agent,
) where
    
import Debug.Trace
import Engine.Core.Classes
import Engine.Core.Coordinate
import Engine.Graphics.Rendering
import Engine.Graphics.Sprite
import Game.Internal(Agent(..), AgentBehaviour(..), World(..))
import Game.Input
import Game.Agents.AgentTypes
import Game.Level.Level
import Constants

{- Data structures -}

{- Classes -}

{- Instances -}
instance Inputable Agent where
    input event a@Agent{behaviour=InputBehaviour inputData} = agent -- trace (show agent) agent
                                                              where agent = a{behaviour=InputBehaviour (input event inputData)}
    input _ a = a

instance Drawable Agent where
    draw a@Agent{position, sprite} = [DrawInstruction position sprite]

{- Functions -}
agent :: AgentType -> Float -> AgentBehaviour -> Agent
agent t s b = Agent t coordinateZero DNone s createEmptySprite b posZero

overflowPosition :: Level -> Coordinate -> Coordinate
overflowPosition l@Level{tiles = Table _ w h} c@(Coordinate x y)
    | x >= halfWidth = overflowPosition l (Coordinate (x - halfWidth * 2) y)
    | x < -halfWidth = overflowPosition l (Coordinate (x + halfWidth * 2) y)
    | y >= halfHeight = overflowPosition l (Coordinate x (y - halfHeight * 2))
    | y < -halfHeight = overflowPosition l (Coordinate x (y + halfHeight * 2))
    | otherwise = c
      where halfWidth = realToFrac (w + 1) * fromInteger tileSize / 2
            halfHeight = realToFrac (h + 1) * fromInteger tileSize / 2

updateAgent :: Float -> Float -> World -> Agent -> Agent
updateAgent dt t world a@Agent{sprite, agentType, position, direction, speed, behaviour, lastTurn}
     = a{sprite=nsprite, direction=ndirection, position=sposition, lastTurn=nlastTurn}
       where
          desiredDirection = updateAgentDirection t world a behaviour
          nsprite = update dt t (updateAgentSprite sprite (agentTypeToSprite ndirection agentType))
          sposition = updateAgentPosition dt world a
          ndirection = adjustDirection desiredDirection (level world) a
          nlastTurn | direction /= ndirection && direction /= DNone = coordinateToTile (tiles (level world)) position
                    | otherwise = lastTurn

-- | Updates agent direction if it is not none. We keep the premise of contineous movement
updateAgentDirection :: Float -> World -> Agent -> AgentBehaviour -> Direction
updateAgentDirection t world agent (AIBehaviour aiFn) = aiFn t agent world
updateAgentDirection _ _ a@Agent{direction} (InputBehaviour (InputData _ newDirection))
    = case newDirection of DNone -> direction
                           _     -> newDirection

-- | Updates agent sprite if it has changed
updateAgentSprite :: Sprite -> Sprite -> Sprite
updateAgentSprite old new | old == new = old
                          | otherwise = new

-- | Update agent position by applying movement in given direction. Also snaps agent orthogonal component to direction to the tile center
updateAgentPosition :: Float -> World -> Agent -> Coordinate
updateAgentPosition dt World{level} a@Agent{position, direction, speed}
    = overflowPosition level (position + deltaTileSnap + deltaDirection)
      where tileCoord = tileToCoordinate (tiles level) (coordinateToTile (tiles level) position)
            orthDir = orthagonalDirection direction
            deltaDesired = coordinateComponent direction ((directionToCoordinate direction) * (fromFloat (speed * dt)))
            deltaTileSnap = coordinateComponent orthDir (tileCoord - position)
            -- Adjust for collision is agent is moving towards a wall
            deltaDirection = adjustCollision direction level position deltaDesired

-- | Adjusts the desired position. If direction is obstructed, it will not be applied.
adjustDirection :: Direction -> Level -> Agent -> Direction
adjustDirection desiredDir level a@Agent{position, direction}
    | (direction /= desiredDir) && not canTurn = adjustDirection direction level a{direction=DNone}
    | isObstructed = adjustDirection direction level a{direction=DNone}
    | otherwise = desiredDir
      where tilePos = coordinateToTile (tiles level) position
            tileCoord = tileToCoordinate (tiles level) tilePos
            nextTilePos = directionToPos desiredDir + tilePos
            nextTileCoord = tileToCoordinate (tiles level) nextTilePos
            orthDir = orthagonalDirection desiredDir
            -- Can turn of close enough to center of the tile
            canTurn = distance (coordinateComponent orthDir nextTileCoord) (coordinateComponent orthDir position) < turnTolerance
            -- Check whether next tile is a wall and agent is close enough to it
            nextTile = tiles level ! nextTilePos
            isObstructed = isWall nextTile && distance (coordinateComponent desiredDir nextTileCoord) (coordinateComponent desiredDir position) <= fromInteger tileSize

-- | Adjusts position by checking for collisions and clamping position to possible travel distance
adjustCollision :: Direction -> Level -> Coordinate -> Coordinate -> Coordinate
adjustCollision dir level pos deltaDesired
    = case nextTile of TileWall _ -> correctedDelta
                       _          -> deltaDesired
      where tilePos = coordinateToTile (tiles level) pos
            -- Delta position to the nearest tile center
            deltaTile = coordinateComponent dir (tileToCoordinate (tiles level) tilePos - pos)
            nextTile = tiles level ! (directionToPos dir + tilePos)
            wallNormal = directionToCoordinate dir * (-1337)
            -- Take farthest option from the wall we are moving towards
            correctedDelta = snd (min (distance deltaDesired wallNormal, deltaDesired) (distance deltaTile wallNormal, deltaTile))