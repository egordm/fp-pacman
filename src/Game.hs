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
import Game.Internal

pacBoi0 = pacman halfScreenSize (InputBehaviour (arrowInput))
pacBoi1 = pacman halfScreenSize (InputBehaviour (wasdInput))

-- TODO: these functions update game state. In the future there is Context around like MainMenu, Game, EndGame.
updateGame :: RoomUpdateFunc
updateGame dt state@GameState{t} = update dt (t + dt) state

inputGame0 (EventKey (SpecialKey KeySpace) Up _ _) state = state{switch = ContextSwitch "b"}
inputGame0 e state = input e state

inputGame1 (EventKey (SpecialKey KeySpace) Up _ _) state = state{switch = ContextSwitch "a"}
inputGame1 e state = input e state

window :: Display
window = InWindow gameName (width, height) (offset, offset)

stdFuncs :: RoomFunctions
stdFuncs = (input, render, updateGame)

playContext context@Context{room,rooms,cInput,cRender,cUpdate} = 
    play window background fps context cRender cInput [cUpdate]

playRoom Room{ initState, rRender, rInput, rUpdate } = 
    play window background fps initState rRender rInput [rUpdate]

start :: IO ()
start = do  level <- readLevel levelClassic
            let init0 = makeState level [pacBoi0]
            let init1 = makeState level [pacBoi0, pacBoi1]
            let room0 = makeRoom init0 (inputGame0, render, updateGame)
            let room1 = makeRoom init1 (inputGame1, render, updateGame)
            let rooms = RoomCollection ("a", room0) [("b", room1)]
            let context = makeContext rooms
            --playFun <- playRoom room0
            playFun <- playContext context
            return playFun