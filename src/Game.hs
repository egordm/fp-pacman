{-# LANGUAGE NamedFieldPuns #-}

module Game (
    start, tester
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
import Game.Level.Level
import Game.Agents.AgentTypes

import Game.Internal

pacBoi0 = pacman (Coordinate 9999 9999) (InputBehaviour (arrowInput))
pacBoi1 = pacman coordinateZero (InputBehaviour (wasdInput))

--TODO: rules in own module, this is just to test
testRule :: Rule
testRule state@GameState{t = time}
    | time > 10 = state{switch = RoomSwitch "a" ResumeRoom}
    | otherwise = state

pacdotsRule :: Rule
pacdotsRule state@GameState{score = sc, world = wrld} = result $ applyRule allAgents (lvl, 0)
    where
        lvl = level wrld
        allAgents = agents wrld
        applyRule :: [Agent] -> (Level, Int) -> (Level, Int)
        applyRule [] l = l
        applyRule (a:as) l = applyRule as $ pacdotsOnPacman a l
        result (nlvl, inc) = state{score = sc + inc, world = wrld{level = nlvl}}

pacdotsOnPacman Agent{agentType = Pacman, position = pos} old@(lvl, inc)
    | onDot = (makeNewLvl, inc + 1)
    | otherwise = old
    where
        table = tiles lvl
        tablePos = coordinateToTile table pos
        tile = table ! tablePos
        onDot = tile == TilePowerup PacDot
        makeNewLvl = lvl{tiles = set table tablePos TileEmpty}
pacdotsOnPacman _ ding = ding

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

tester = do
    lvl <- readLevel levelClassic
    let x = (tiles lvl) ! (Pos 0 (-1))
    return x

start :: IO ()
start = do  level <- readLevel levelClassic
            let init0 = makeState level [pacBoi0]
            let init1 = makeState level [pacBoi0, pacBoi1]
            let room0 = makeRoom init0 [pacdotsRule] (inputGame0, render, updateGame)
            let room1 = makeRoom init1 [testRule] (inputGame1, render, updateGame)
            let rooms = RoomCollection ("a", room0) [("b", room1)]
            let context = makeContext rooms
            playFun <- playContext stdPlay context
            return playFun