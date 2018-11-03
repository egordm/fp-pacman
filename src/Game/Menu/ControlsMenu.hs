module Game.Menu.ControlsMenu(
    controlsMenu
) where

import Engine.Base
import Graphics.Gloss.Game
import Game.UI.Base
import Game.Context.SwitchRoom
import Game.Context.Room
import Game.Menu.MenuShared

mainButtonF (EventKey (SpecialKey KeyEnter) Up _ _) b = b{itemSwitch = RoomSwitch "main" ReloadRoom}
mainButtonF _ b = b

uiElements = [
    makeLabel "contols menu" (Coordinate 0 (-240)) Center,
    makeLabel "\"w\" and \"s\" to select-" (Coordinate 0 (-120)) Center,
    makeLabel "-items in the menu" (Coordinate 0 (-90)) Center,
    makeLabel "\"enter\" to activate-" (Coordinate 0 (-40)) Center,
    makeLabel "-a menu item" (Coordinate 0 (-10)) Center,
    makeLabel "\"wasd\" to move pacman" (Coordinate 0 (40)) Center,
    makeLabel "\"p\" to pause the game" (Coordinate 0 (80)) Center,
    makeButton "main menu" "-main menu-" 0 mainButtonF (Coordinate 0 240)]

controlsMenu = makeMenu uiElements [basicSelectorRule 0]
  