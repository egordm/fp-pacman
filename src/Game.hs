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
import Game.Level
import Game.Room

pacBoi0 = pacman halfScreenSize (InputBehaviour (arrowInput))
pacBoi1 = pacman halfScreenSize (InputBehaviour (wasdInput))

-- TODO: these functions update game state. In the future there is Context around like MainMenu, Game, EndGame.
updateGame :: RoomUpdateFunc
updateGame dt state@GameState{t} = update dt (t + dt) state

window :: Display
window = InWindow gameName (width, height) (offset, offset)

stdFuncs :: RoomFunctions
stdFuncs = (input, render, updateGame)

playRoom Room{ initState, fRender, fInput, fUpdate } = 
    play window background fps initState fRender fInput [fUpdate]

start :: IO ()
start = do  level <- readLevel levelClassic
            let init0 = makeState level [pacBoi0]
            let init1 = makeState level [pacBoi0, pacBoi1]
            let room0 = makeRoom init0 stdFuncs
            let room1 = makeRoom init1 stdFuncs
            playFun <- playRoom room1
            return playFun