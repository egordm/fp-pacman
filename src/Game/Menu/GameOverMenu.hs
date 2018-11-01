module Game.Menu.GameOverMenu (
    gameOverMenu
) where

import Engine.Base
import Graphics.Gloss.Game
import Game.UI.Base
import Game.Context.SwitchRoom
import Game.Context.Room

replayButtonF (EventKey (SpecialKey KeyEnter) Up _ _) b = b{itemSwitch = RoomSwitch "classic" ReloadRoom}
replayButtonF _ b = b

overMenuList = [makeLabel "game over!" (Coordinate 0 (-200)) Center,
            makeButton "replay" "-replay-" 0 replayButtonF (Coordinate 0 0)]

gameOverMenu = makeMenu overMenuList []