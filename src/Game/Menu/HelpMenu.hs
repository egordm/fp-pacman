module Game.Menu.HelpMenu(
    helpMenu
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
    makeLabel "help menu" (Coordinate 0 (-240)) Center,
    makeLabel "\"w\" and \"s\" to select-" (Coordinate 0 (-200)) Center,
    makeLabel "-items in the menu" (Coordinate 0 (-170)) Center,
    makeLabel "\"enter\" to activate-" (Coordinate 0 (-120)) Center,
    makeLabel "-a menu item" (Coordinate 0 (-90)) Center,
    makeLabel "\"wasd\" to move pacman" (Coordinate 0 (-40)) Center,
    makeLabel "eat all the pacdots" (Coordinate 0 (0)) Center,
    makeLabel "do not touch a ghost-" (Coordinate 0 (40)) Center,
    makeLabel "-you die and lose a live" (Coordinate 0 (70)) Center,
    makeLabel "if you die with 0 lives-" (Coordinate 0 (120)) Center,
    makeLabel "-than it is game over" (Coordinate 0 (150)) Center,
    makeLabel "eat a cherry to eat ghosts-" (Coordinate 0 (200)) Center,
    makeLabel "-for a while" (Coordinate 0 (230)) Center,
    makeButton "main menu" "-main menu-" 0 mainButtonF (Coordinate 0 260)]

helpMenu = makeMenu uiElements [basicSelectorRule 0]
        