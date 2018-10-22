{-# LANGUAGE NamedFieldPuns #-}

module Game (
    start
) where

import Graphics.Gloss(Picture)
import Graphics.Gloss.Game
import Constants
import Resources
import Engine.Core.Coordinate
import Game.World
import Game.GameState
import Game.Input
import Game.Agent
import Game.Agents.Pacman


testPacman = pacman halfScreenSize (InputBehaviour (arrowInput))

{- Functions -}
initialState :: GameState
initialState = GameState 0 (World [testPacman])

-- TODO: these functions update game state. In the future there is Context around like MainMenu, Game, EndGame.
updateGame :: Float -> GameState -> GameState
updateGame dt state@GameState{t} = update dt (t + dt) state

checkInput :: Event -> GameState -> GameState
checkInput e state = input e state

renderGame :: GameState -> Picture
renderGame = render

window :: Display
window = InWindow gameName (width, height) (offset, offset)

start :: IO ()
start = play window background fps initialState renderGame checkInput [updateGame]