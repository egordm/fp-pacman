module Game.Agents.AgentTypes (
    AgentType(..),
    GhostType(..),
    agentTypeToSprite,
    agentTypeToMarker,
    agentTypeToSpeed,
    isInScatterMode,
    isGhost,
    ghost,
    ghostEmpty
) where

import Engine.Core.Base
import Engine.Graphics.Base
import Game.Level.Level(Marker(..))
import Resources
import Constants

{- Data structures -}
data GhostType = Blinky | Pinky | Inky | Clyde deriving (Eq, Ord, Show)

data AgentType = Pacman {
                   died :: Bool
                 } | Ghost {
                   ghostType :: GhostType,
                   homePosition :: Coordinate,
                   dotsUntilRelease :: Int,
                   scatterTicks :: Float,
                   died :: Bool,
                   caged :: Bool
                 } deriving (Show)

{- Classes -}


{- Instances -}
instance Eq AgentType where
    Pacman{} == Pacman{} = True
    Ghost{ghostType=ga} == Ghost{ghostType=gb} = ga == gb
    _ == _ = False

instance Updateable AgentType where
    update dt t at@Pacman{} = at
    update dt t at@Ghost{} = at{scatterTicks=max 0 (scatterTicks at - dt)}

instance Resetable AgentType where
    reset a@Pacman{} = a{died=False}
    reset a@Ghost{} = a{died=False, scatterTicks=0}

{- Functions -}
ghost :: GhostType -> Coordinate -> Bool -> Int -> AgentType
ghost t h c d = Ghost t h d 0 False c

ghostEmpty :: GhostType -> AgentType
ghostEmpty t = ghost t coordZ False 0

isInScatterMode :: AgentType -> Bool
isInScatterMode Pacman{} = False
isInScatterMode Ghost{scatterTicks} = scatterTicks > 0

agentTypeToSpeed :: AgentType -> Float -> Float
agentTypeToSpeed at defaultSpeed | died at = ghostSpeedDead
                                 | isInScatterMode at = ghostSpeedScatter
                                 | otherwise = defaultSpeed

isGhost :: AgentType -> Bool
isGhost Ghost{} = True
isGhost _ = False

agentTypeToMarker :: AgentType -> Marker
agentTypeToMarker agent = case agent of
    Pacman{}                -> Marker 'M'
    Ghost{ghostType=Blinky} -> Marker 'B'
    Ghost{ghostType=Pinky}  -> Marker 'P'
    Ghost{ghostType=Inky}   -> Marker 'I'
    Ghost{ghostType=Clyde}  -> Marker 'C'
    _                       -> Marker '_'

agentTypeToSprite :: Direction -> AgentType -> Sprite
agentTypeToSprite direction Pacman{died=True} = spritePacmanDie
agentTypeToSprite direction Pacman{}
  = case direction of
    DNone  -> spritePacmanStill
    DUp    -> spritePacmanUp
    DDown  -> spritePacmanDown
    DLeft  -> spritePacmanLeft
    DRight -> spritePacmanRight
agentTypeToSprite direction Ghost{died=True}
  = case direction of
    DNone  -> spriteEyesUp
    DUp    -> spriteEyesUp
    DDown  -> spriteEyesDown
    DLeft  -> spriteEyesLeft
    DRight -> spriteEyesRight
agentTypeToSprite direction Ghost{ghostType, scatterTicks} | (scatterTicks > scatterModeEnding) = spriteScatter
                                                           | (scatterTicks > 0) = spriteScatterEnding
                                                           | otherwise = ghostTypeToSprite direction ghostType

ghostTypeToSprite :: Direction -> GhostType -> Sprite
ghostTypeToSprite direction Blinky
  = case direction of
    DNone  -> spriteBlinkyStill
    DUp    -> spriteBlinkyUp
    DDown  -> spriteBlinkyDown
    DLeft  -> spriteBlinkyLeft
    DRight -> spriteBlinkyRight
ghostTypeToSprite direction Pinky
  = case direction of
    DNone  -> spritePinkyStill
    DUp    -> spritePinkyUp
    DDown  -> spritePinkyDown
    DLeft  -> spritePinkyLeft
    DRight -> spritePinkyRight
ghostTypeToSprite direction Inky
  = case direction of
    DNone  -> spriteInkyStill
    DUp    -> spriteInkyUp
    DDown  -> spriteInkyDown
    DLeft  -> spriteInkyLeft
    DRight -> spriteInkyRight
ghostTypeToSprite direction Clyde
  = case direction of
    DNone  -> spriteClydeStill
    DUp    -> spriteClydeUp
    DDown  -> spriteClydeDown
    DLeft  -> spriteClydeLeft
    DRight -> spriteClydeRight