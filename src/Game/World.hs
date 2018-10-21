module Game.World (
    World(..),
    Updateable(..),
    Renderable(..)
) where

import Engine.Graphics.Sprite
import Engine.Graphics.Rendering
import Engine.Core.Classes
import Game.Internal(World(..))
import Engine.Core.Coordinate

{- Data structures -}

{- Classes -}

{- Instances -}
instance Updateable World where
    update dt t (World test agents) = World (update dt t test) agents

instance Renderable World where
    render (World test agents) = render (DrawInstruction halfScreenSize test)

{- Functions -}

