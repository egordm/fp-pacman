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
import qualified Game.Menu.Base as Menu
import qualified Game.GameModes.Base as Mode

window :: Display
window = InWindow gameName (width, height) (offset, offset)

playFn = playIO window background fps

start :: IO ()
start = do
    classic <- Mode.classicMode
    let rooms = RoomCollection ("classic", classic) [("gameover", Menu.gameOverMenu)]
    SDL.initialize [SDL.InitAudio]
    let chunkSz = 256 in Mix.withAudio Mix.defaultAudio chunkSz $ do
        sounds <- loadSounds
        let context = makeContext rooms sounds
        playContext playFn context
    SDL.quit