module Game.GameModes.Classic (
    classicMode
) where

import Game.GameModes.CommonShared
import Engine.Base
import Game.Agents.Base
import Game.Level.Base
import Game.Structure.Base
import Game.Context.Base
import Resources

classicMode = do
    level <- readLevel levelClassic
    let init = gamestate level [stdPacman, blinky, pinky, inky, clyde]
    return $ makeRoom init stdRules [pauseOnP] stdUpdateGame