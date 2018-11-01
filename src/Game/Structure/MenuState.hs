module Game.Structure.MenuState(
    MenuState(..)
) where

import Engine.Base
import Game.UI.Base

data MenuState = MenuState{items :: [MenuItem], selector :: Int}