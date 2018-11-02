module Game.Menu.MenuTut (
    menuTut
) where

import Engine.Base
import Graphics.Gloss.Game
import Game.UI.Base
import Game.Context.SwitchRoom
import Game.Context.Room
import Game.Menu.MenuShared

mainButtonF (EventKey (SpecialKey KeyEnter) Up _ _) b = b{itemSwitch = RoomSwitch "main" ReloadRoom}
mainButtonF _ b = b

fakeButtonF _ b = b

uiElements = [
    makeLabel "pacman" (Coordinate 0 (-200)) Center,
    makeLabel "made in haskell" (Coordinate 0 (-160)) Center,
    makeLabel "by" (Coordinate 0 (-140)) Center,
    makeLabel "egor dmitriev" (Coordinate 0 (-100)) Center,
    makeLabel "cody bloemhard" (Coordinate 0 (-60)) Center,
    makeLabel "use \"w\" and \"s\" to select-" (Coordinate 0 (-20)) Center,
    makeLabel "-menu items" (Coordinate 0 (20)) Center,
    makeLabel "and enter to press buttons" (Coordinate 0 (60)) Center,
    makeButton "fake button" "-fake button-" 0 fakeButtonF (Coordinate 0 100),
    makeButton "not this one either" "-not this one either-" 1 fakeButtonF (Coordinate 0 140),
    makeButton "this one to play" "-this one to play-" 2 mainButtonF (Coordinate 0 180),
    makeButton "but this one not" "-but this one not-" 3 fakeButtonF (Coordinate 0 220)]

menuTut = makeMenu uiElements [basicSelectorRule 3]
