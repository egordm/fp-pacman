module Game.Menu.PauseMenu (
    pauseMenu
) where

import Engine.Base
import Graphics.Gloss.Game
import Game.UI.Base
import Game.Context.SwitchRoom
import Game.Context.Room
import Game.Menu.MenuShared

continueButtonF (EventKey (SpecialKey KeyEnter) Up _ _) b = b{itemSwitch = RoomSwitch "classic" ResumeRoom}
continueButtonF _ b = b

mainButtonF (EventKey (SpecialKey KeyEnter) Up _ _) b = b{itemSwitch = RoomSwitch "main" ReloadRoom}
mainButtonF _ b = b

uiElements = [
    makeLabel "game is paused" (Coordinate 0 (-200)) Center,
    makeButton "continue" "-continue-" 0 continueButtonF (Coordinate 0 0),
    makeButton "quit to main menu" "-quit to main menu-" 1 mainButtonF (Coordinate 0 40)]

pauseMenu = makeMenu uiElements [basicSelectorRule 1]
    