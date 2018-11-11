module Game.Structure.MenuState(
    MenuState(..), makeMenuState
) where

import Engine.Base
import Game.UI.Base
import Game.Context.Persistant
import Game.File.Base

data MenuState = MenuState{
    items :: [MenuItem], 
    selector :: Int,
    menuOldPersistant :: Persistant,
    menuNewPersistant :: Persistant
}

makeMenuState items selec = MenuState items selec emptyPersistant emptyPersistant