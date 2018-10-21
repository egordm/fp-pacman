module Engine.Core.GameObject (
    GameObject(..)
) where

import Engine.Core.Classes
import Engine.Graphics.Rendering
import Game.World

{- Data structures -}


{- Classes -}
class (Drawable a) => GameObject a where
    update :: Float -> GameState -> a -> a


{- Instances -}


{- Functions -}

