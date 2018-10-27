{-# LANGUAGE NamedFieldPuns #-}

module Game.Agents.AgentTypes (
    AgentType(..),
    GhostType(..),
    agentTypeToSprite,
    agentTypeToMarker,
    ghost
) where

import Engine.Core.Base
import Engine.Graphics.Base
import Game.Level.Level(Marker(..))
import Resources

{- Data structures -}
data GhostType = Blinky | Pinky | Inky | Clyde deriving (Eq, Ord, Show)

data AgentType = Pacman
               | Ghost {
                  ghostType :: GhostType,
                  scatterTicks :: Int,
                  died :: Bool,
                  homePosition :: Coordinate
               } deriving (Show)

{- Classes -}


{- Instances -}
instance Eq AgentType where
    Pacman == Pacman = True
    Ghost{ghostType=ga} == Ghost{ghostType=gb} = ga == gb
    _ == _ = False

{- Functions -}
ghost :: GhostType -> Coordinate -> AgentType
ghost t h = Ghost t 0 False h

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
agentTypeToSprite direction Ghost{ghostType, scatterTicks} | (scatterTicks > 0) = spriteScatterRight
                                                           | otherwise = ghostTypeToSprite direction ghostType

ghostTypeToSprite :: Direction -> GhostType -> Sprite
ghostTypeToSprite direction Blinky = case direction of
    DNone  -> spriteBlinkyStill
    DUp    -> spriteBlinkyUp
    DDown  -> spriteBlinkyDown
    DLeft  -> spriteBlinkyLeft
    DRight -> spriteBlinkyRight
ghostTypeToSprite direction Pinky = case direction of
    DNone  -> spritePinkyStill
    DUp    -> spritePinkyUp
    DDown  -> spritePinkyDown
    DLeft  -> spritePinkyLeft
    DRight -> spritePinkyRight
ghostTypeToSprite direction Inky = case direction of
    DNone  -> spriteInkyStill
    DUp    -> spriteInkyUp
    DDown  -> spriteInkyDown
    DLeft  -> spriteInkyLeft
    DRight -> spriteInkyRight
ghostTypeToSprite direction Clyde = case direction of
    DNone  -> spriteClydeStill
    DUp    -> spriteClydeUp
    DDown  -> spriteClydeDown
    DLeft  -> spriteClydeLeft
    DRight -> spriteClydeRight