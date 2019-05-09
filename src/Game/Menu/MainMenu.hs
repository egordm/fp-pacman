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

playButtonRndF (EventKey (SpecialKey KeyEnter) Up _ _) b = b{itemSwitch = RoomSwitch "classicRnd" ReloadRoom}
playButtonRndF _ b = b

helpButtonF (EventKey (SpecialKey KeyEnter) Up _ _) b = b{itemSwitch = RoomSwitch "help" ReloadRoom}
helpButtonF _ b = b

controlsHelpF (EventKey (SpecialKey KeyEnter) Up _ _) b = b{itemSwitch = RoomSwitch "controls" ReloadRoom}
controlsHelpF _ b = b

uiElements = [
    makeLabel "pacman" (Coordinate 0 (-200)) Center,
    makeLabel "main menu" (Coordinate 0 (-160)) Center,
    makeButton "play classic" "-play classic-" 0 playButtonF (Coordinate 0 0),
    makeButton "play w random" "-play w random-" 1 playButtonRndF (Coordinate 0 40),
    makeButton "help" "-help-" 2 helpButtonF (Coordinate 0 80),
    makeButton "controls" "-controls-" 3 controlsHelpF (Coordinate 0 120)]

mainMenu = makeMenu uiElements [basicSelectorRule 3]