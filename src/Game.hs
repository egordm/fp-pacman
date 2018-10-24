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

pacBoi0 = pacman coordinateZero (InputBehaviour (arrowInput))
pacBoi1 = pacman coordinateZero (InputBehaviour (wasdInput))

-- TODO: these functions update game state. In the future there is Context around like MainMenu, Game, EndGame.
updateGame :: RoomUpdateFunc
updateGame dt state@GameState{t} = update dt (t + dt) state

window :: Display
window = InWindow gameName (width, height) (offset, offset)

stdFuncs :: RoomFunctions
stdFuncs = (input, render, updateGame)

playContext context@Context{room,initRoom,cInput,cRender,cUpdate} = 
    play window background fps context cRender cInput [cUpdate]

playRoom Room{ initState, rRender, rInput, rUpdate } = 
    play window background fps initState rRender rInput [rUpdate]

start :: IO ()
start = do  level <- readLevel levelClassic
            let init0 = makeState level [pacBoi0]
            let init1 = makeState level [pacBoi0, pacBoi1]
            let room0 = makeRoom init0 stdFuncs
            let room1 = makeRoom init1 stdFuncs
            let cont0 = makeContext room0
            let cont1 = makeContext room1
            --playFun <- playRoom room0
            playFun <- playContext cont1
            return playFun