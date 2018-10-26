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
import Game.Level.Loading
import Game.Room
import Game.Context
import Game.SwitchRoom
import Game.Rule

pacBoi0 = pacman (Coordinate 9999 9999) (InputBehaviour (arrowInput))
pacBoi1 = pacman coordinateZero (InputBehaviour (wasdInput))

--TODO: rules in own module, this is just to test
testRule :: Rule
testRule state@GameState{t = time}
    | time > 10 = state{switch = RoomSwitch "a" ResumeRoom}
    | otherwise = state

updateGame :: RoomUpdateFunc
updateGame dt state@GameState{t} = update dt (t + dt) state

inputGame0 (EventKey (SpecialKey KeySpace) Up _ _) state = state{switch = RoomSwitch "b" ReloadRoom}
inputGame0 e state = input e state

inputGame1 (EventKey (SpecialKey KeySpace) Up _ _) state = state{switch = RoomSwitch "a" ResumeRoom}
inputGame1 e state = input e state

window :: Display
window = InWindow gameName (width, height) (offset, offset)

stdFuncs :: RoomFunctions
stdFuncs = (input, render, updateGame)

stdPlay = play window background fps

start :: IO ()
start = do  level <- readLevel levelClassic
            let init0 = makeState level [pacBoi0]
            let init1 = makeState level [pacBoi0, pacBoi1]
            let room0 = makeRoom init0 [] (inputGame0, render, updateGame)
            let room1 = makeRoom init1 [testRule] (inputGame1, render, updateGame)
            let rooms = RoomCollection ("a", room0) [("b", room1)]
            let context = makeContext rooms
            playFun <- playContext stdPlay context
            return playFun