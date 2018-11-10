module Game.GameModes.Classic (
    classicMode,
    classicCustomMode
) where

import System.Random
import Game.GameModes.CommonShared
import Engine.Base
import Game.Agents.Base
import Game.Level.Base
import Game.Structure.Base
import Game.Context.Base
import Resources

classicMode = do
    level <- readLevel levelClassic
    rng <- getStdGen
    let init = gamestate level [stdPacman, blinky, pinky, inky, clyde] rng
    return $ makeRoom init stdRules [pauseOnP] stdUpdateGame

classicCustomMode = do
    level <- readLevel levelClassic
    rng <- getStdGen
    let init = gamestate level [stdPacman, blinky, pinky, inky, clyde, randy] rng
    return $ makeRoom init stdRules [pauseOnP] stdUpdateGame