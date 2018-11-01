module Game.Menu.MenuShared (
    basicSelectorRule
) where

import Engine.Base
import Graphics.Gloss.Game
import Game.UI.Base
import Game.Context.SwitchRoom
import Game.Context.Room
import Game.Structure.Base

cycleClamp :: Int -> Int -> Int -> Int
cycleClamp x min max
    | x < min = max
    | x > max = min
    | otherwise = x 

basicSelectorRule maxnr (EventKey (Char 's') Up _ _) ms@MenuState{selector = s} = ms{selector = cycleClamp (s + 1) 0 maxnr}
basicSelectorRule maxnr (EventKey (Char 'w') Up _ _) ms@MenuState{selector = s} = ms{selector = cycleClamp (s - 1) 0 maxnr}
basicSelectorRule _ _ ms = ms