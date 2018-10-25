module Game.Agents.AgentTypes (
    AgentType(..),
    GhostType(..),
    agentTypeToSprite,
    agentTypeToMarker
) where

import Engine.Core.Coordinate
import Engine.Graphics.Sprite
import Game.Level.Level(Marker(..))
import Resources

{- Data structures -}
data GhostType = Blinky | Pinky | Inky | Clyde deriving (Eq, Ord, Show)

data AgentType = Pacman
               | Ghost {
                  ghostType :: GhostType,
                  scatterMode :: Bool,
                  died :: Bool
               } deriving (Eq, Show)

{- Classes -}


{- Instances -}


{- Functions -}
agentTypeToMarker :: AgentType -> Marker
agentTypeToMarker agent = case agent of Pacman                  -> Marker 'M'
                                        Ghost{ghostType=Blinky} -> Marker 'B'
                                        Ghost{ghostType=Pinky}  -> Marker 'P'
                                        Ghost{ghostType=Inky}   -> Marker 'I'
                                        Ghost{ghostType=Clyde}  -> Marker 'C'
                                        _                       -> Marker '_'

agentTypeToSprite :: Direction -> AgentType -> Sprite
agentTypeToSprite direction Pacman = case direction of
    DNone  -> spritePacmanStill
    DUp    -> spritePacmanUp
    DDown  -> spritePacmanDown
    DLeft  -> spritePacmanLeft
    DRight -> spritePacmanRight
agentTypeToSprite direction Ghost{died=True} = case direction of
    DNone  -> spriteEyesUp
    DUp    -> spriteEyesUp
    DDown  -> spriteEyesDown
    DLeft  -> spriteEyesLeft
    DRight -> spriteEyesRight
agentTypeToSprite direction Ghost{scatterMode=True} = spriteScatterRight
agentTypeToSprite direction Ghost{ghostType=Blinky} = case direction of
    DNone  -> spriteBlinkyStill
    DUp    -> spriteBlinkyUp
    DDown  -> spriteBlinkyDown
    DLeft  -> spriteBlinkyLeft
    DRight -> spriteBlinkyRight
agentTypeToSprite direction Ghost{ghostType=Pinky} = case direction of
    DNone  -> spritePinkyStill
    DUp    -> spritePinkyUp
    DDown  -> spritePinkyDown
    DLeft  -> spritePinkyLeft
    DRight -> spritePinkyRight
agentTypeToSprite direction Ghost{ghostType=Inky} = case direction of
    DNone  -> spriteInkyStill
    DUp    -> spriteInkyUp
    DDown  -> spriteInkyDown
    DLeft  -> spriteInkyLeft
    DRight -> spriteInkyRight
agentTypeToSprite direction Ghost{ghostType=Clyde} = case direction of
    DNone  -> spriteClydeStill
    DUp    -> spriteClydeUp
    DDown  -> spriteClydeDown
    DLeft  -> spriteClydeLeft
    DRight -> spriteClydeRight