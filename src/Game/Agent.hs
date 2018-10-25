{-# LANGUAGE NamedFieldPuns #-}

module Game.Agent (
    Agent(..),
    Drawable(..),
    AgentBehaviour(..),
    updateAgent,
    updateAgentDirection
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
updateAgent :: Float -> Float -> World -> Agent -> Agent
updateAgent dt t world a@Agent{sprite, agentType, position, direction, speed, behaviour}
     = a{sprite=nsprite, direction=ndirection, position=sposition}
       where
          desiredDirection = updateAgentDirection t world a behaviour
          nsprite = update dt t (updateAgentSprite sprite (agentTypeToSprite ndirection agentType))
          sposition = updateAgentPosition dt world a
          ndirection = adjustDirection desiredDirection (level world) a


updateAgentDirection :: Float -> World -> Agent -> AgentBehaviour -> Direction
updateAgentDirection t world agent (AIBehaviour aiFn) = aiFn t agent world
updateAgentDirection _ _ a@Agent{direction} (InputBehaviour (InputData _ newDirection))
    = case newDirection of DNone -> direction; _ -> newDirection

updateAgentSprite :: Sprite -> Sprite -> Sprite
updateAgentSprite old new | old == new = old
                          | otherwise = new

updateAgentPosition :: Float -> World -> Agent -> Coordinate
updateAgentPosition dt World{level} a@Agent{position, direction, speed}
    = position + deltaTileSnap + deltaDirection
      where tileCoord = tileToCoordinate (tiles level) (coordinateToTile (tiles level) position)
            orthDir = orthagonalDirection direction
            deltaDesired = coordinateComponent direction ((directionToCoordinate direction) * (fromFloat (speed * dt)))
            deltaTileSnap = coordinateComponent orthDir (tileCoord - position)
            deltaDirection = adjustCollision direction level position deltaDesired

--            canTurn = distance tilePos position < epsilon

adjustDirection desiredDir level a@Agent{position, direction}
  --  | (direction /= desiredDir) && not canTurn = adjustDirection direction level a{direction=DNone}
    | isObstructed = adjustDirection direction level a{direction=DNone}
    | otherwise = desiredDir
      where tilePos = coordinateToTile (tiles level) position
            tileCoord = tileToCoordinate (tiles level) tilePos
            nextTilePos = directionToPos desiredDir + tilePos
            nextTileCoord = tileToCoordinate (tiles level) nextTilePos
            orthDir = orthagonalDirection desiredDir
            canTurn = distance (coordinateComponent orthDir nextTileCoord) (coordinateComponent orthDir position) < turnTolerance
            nextTile = tiles level ! nextTilePos
            isObstructed = isWall nextTile && distance (coordinateComponent desiredDir nextTileCoord) (coordinateComponent desiredDir position) <= fromInteger tileSize

adjustCollision :: Direction -> Level -> Coordinate -> Coordinate -> Coordinate
adjustCollision dir level pos deltaDesired
    = case nextTile of TileWall _ -> correctedDelta
                       _          -> deltaDesired
      where tilePos = coordinateToTile (tiles level) pos
            deltaTile = coordinateComponent dir (tileToCoordinate (tiles level) tilePos - pos)
            nextTile = tiles level ! (directionToPos dir + tilePos)
            wallNormal = directionToCoordinate dir * (-1337)
            correctedDelta = snd (min (distance deltaDesired wallNormal, deltaDesired) (distance deltaTile wallNormal, deltaTile))