module Engine.Graphics.Rendering (
    renderInstructions,
    DrawInstruction(..),
    Renderable(..),
    Updateable(..),
    Drawable(..)
) where

import Graphics.Gloss
import Engine.Core.Classes
import Engine.Core.Base
import Engine.Graphics.Sprite

{- Data structures -}
data DrawInstruction = DrawInstruction Coordinate Sprite

{- Classes -}
class Drawable a where
    draw :: a -> [DrawInstruction]

{- Instances -}
instance Renderable DrawInstruction where
    render (DrawInstruction (Coordinate x y) sprite) = translate x (-y) (render sprite)

{- Functions -}
renderInstructions :: [DrawInstruction] -> Picture
renderInstructions instructions = pictures (map render instructions)





