module Game.GameModes.CommonShared (
    stdPacman,
    stdUpdateGame,
    reloadOnSpace, pauseOnP,
    stdRules
) where

import Engine.Base
import Game.Agents.Base
import Game.UI.Base
import Game.Rules.Base
import Graphics.Gloss.Game
import Game.Context.Base
import Game.Structure.Base
import Game.Input.Base

stdPacman = pacman (Coordinate 9999 9999) (InputBehaviour (arrowInput))

stdUpdateGame :: RoomUpdateFunc
stdUpdateGame dt state@GameState{t} = update dt (t + dt) state

reloadOnSpace :: Event -> GameState -> GameState
reloadOnSpace (EventKey (SpecialKey KeySpace) Up _ _) state = state{switch = RoomReload}
reloadOnSpace _ state = state

pauseOnP (EventKey (Char 'p') Up _ _) state = state{switch = RoomSwitch "pause" ReloadRoom}
pauseOnP _ state = state

stdRules :: [GameRule]
stdRules = [rulePacmanDotConsume, ruleGhostCatchPacman, rulePacmanDiedRestart, rulePacmanPowerpillConsume,
    rulePacmanEatGhost, ruleGhostRevives, ruleGhostRelease, ruleBackgroundSound]