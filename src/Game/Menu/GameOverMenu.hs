module Game.Menu.GameOverMenu (
    gameOverMenu
) where

import Engine.Base
import Graphics.Gloss.Game
import Game.UI.Base
import Game.Context.SwitchRoom
import Game.Context.Room
import Game.Menu.MenuShared

replayButtonF (EventKey (SpecialKey KeyEnter) Up _ _) b = b{itemSwitch = RoomSwitch "classic" ReloadRoom}
replayButtonF _ b = b

mainButtonF (EventKey (SpecialKey KeyEnter) Up _ _) b = b{itemSwitch = RoomSwitch "main" ReloadRoom}
mainButtonF _ b = b

uiElements = [
    makeLabel "game over!" (Coordinate 0 (-200)) Center,
    makeButton "replay" "-replay-" 0 replayButtonF (Coordinate 0 0),
    makeButton "main menu" "-main menu-" 1 mainButtonF (Coordinate 0 40)]

gameOverMenu = makeMenu uiElements [basicSelectorRule 1]
