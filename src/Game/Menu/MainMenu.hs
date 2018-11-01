module Game.Menu.MainMenu (
    mainMenu
) where

import Engine.Base
import Graphics.Gloss.Game
import Game.UI.Base
import Game.Context.SwitchRoom
import Game.Context.Room
import Game.Menu.MenuShared

playButtonF (EventKey (SpecialKey KeyEnter) Up _ _) b = b{itemSwitch = RoomSwitch "classic" ReloadRoom}
playButtonF _ b = b

helpButtonF (EventKey (SpecialKey KeyEnter) Up _ _) b = b{itemSwitch = RoomSwitch "help" ReloadRoom}
helpButtonF _ b = b

uiElements = [
    makeLabel "pacman" (Coordinate 0 (-200)) Center,
    makeLabel "main menu" (Coordinate 0 (-160)) Center,
    makeButton "play" "-play-" 0 playButtonF (Coordinate 0 0),
    makeButton "help" "-help-" 1 helpButtonF (Coordinate 0 40)]

mainMenu = makeMenu uiElements [basicSelectorRule 1]