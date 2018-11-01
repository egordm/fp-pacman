module Game (
    start
) where

import Graphics.Gloss(Picture)
import Graphics.Gloss.Game
import Graphics.Gloss.Interface.IO.Game
import qualified SDL
import qualified SDL.Mixer as Mix
import Constants
import Resources
import Engine.Base
import Game.Base
import Game.UI.Base
import qualified Game.Menu.Base as Menu

pacBoi0 = pacman (Coordinate 9999 9999) (InputBehaviour (arrowInput))
pacBoi1 = pacman coordZ (InputBehaviour (wasdInput))

updateGame :: RoomUpdateFunc
updateGame dt state@GameState{t} = update dt (t + dt) state

inputGame0 (EventKey (SpecialKey KeySpace) Up _ _) state = state{switch = RoomSwitch "b" ReloadRoom}
inputGame0 e state = input e state

inputGame1 (EventKey (SpecialKey KeySpace) Up _ _) state = state{switch = RoomSwitch "a" ResumeRoom}
inputGame1 e state = input e state

stdRules = [rulePacmanDotConsume, ruleGhostCatchPacman, rulePacmanDiedRestart, rulePacmanPowerpillConsume,
  rulePacmanEatGhost, ruleGhostRevives, ruleGhostRelease, ruleBackgroundSound]

window :: Display
window = InWindow gameName (width, height) (offset, offset)

playFn = playIO window background fps

start :: IO ()
start = do
    level <- readLevel levelClassic
    let init0 = gamestate level [pacBoi0, blinky, pinky, inky, clyde]
    let init1 = gamestate level [pacBoi0, pacBoi1]
    let room0 = makeRoom init0 stdRules [inputGame0] updateGame
    let room1 = makeRoom init1 stdRules [inputGame1] updateGame
    let rooms = RoomCollection ("a", room0) [("b", room1),("gameover", Menu.gameOverMenu)]

    SDL.initialize [SDL.InitAudio]
    let chunkSz = 256 in Mix.withAudio Mix.defaultAudio chunkSz $ do
        sounds <- loadSounds
        let context = makeContext rooms sounds
        playContext playFn context
    SDL.quit