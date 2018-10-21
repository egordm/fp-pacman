module Game.World (
    World(..),
    Updateable(..),
    Renderable(..)
) where

import Engine.Graphics.Sprite
import Engine.Graphics.Rendering
import Engine.Core.Classes
import Engine.Core.Coordinate


{- Data structures -}
data World = World {
                 test :: Sprite -- TODO: temporary. while we have nothing
             } deriving (Show)

{- Classes -}


{- Instances -}
instance Updateable World where
    update dt t (World test) = World (update dt t test)

instance Renderable World where
    render (World test) = render (DrawInstruction halfScreenSize test)

{- Functions -}

