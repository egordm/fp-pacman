module Game (
    start
) where

import Graphics.Gloss(Picture)
import Graphics.Gloss.Game
import Constants
import Resources
import Engine.Core.Base
import Game.Base

pacBoi0 = pacman (Coordinate 9999 9999) (InputBehaviour (arrowInput))
pacBoi1 = pacman coordZ (InputBehaviour (wasdInput))

updateGame :: RoomUpdateFunc
updateGame dt state@GameState{t} = update dt (t + dt) state

inputGame0 (EventKey (SpecialKey KeySpace) Up _ _) state = state{switch = RoomSwitch "b" ReloadRoom}
inputGame0 e state = input e state

inputGame1 (EventKey (SpecialKey KeySpace) Up _ _) state = state{switch = RoomSwitch "a" ResumeRoom}
inputGame1 e state = input e state


stdFuncs :: RoomFunctions
stdFuncs = (input, render, updateGame)

stdRules = [rulePacmanDotConsume, ruleGhostCatchPacman]

window :: Display
window = InWindow gameName (width, height) (offset, offset)

stdPlay = play window background fps

start :: IO ()
start = do  level <- readLevel levelClassic
            let init0 = makeState level [pacBoi0, blinky, pinky, inky, clyde]
            let init1 = makeState level [pacBoi0, pacBoi1]
            let room0 = makeRoom init0 stdRules (inputGame0, render, updateGame)
            let room1 = makeRoom init1 stdRules (inputGame1, render, updateGame)
            let rooms = RoomCollection ("a", room0) [("b", room1)]
            let context = makeContext rooms
            playFun <- playContext stdPlay context
            return playFun