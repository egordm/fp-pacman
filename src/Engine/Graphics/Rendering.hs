module Engine.Graphics.Rendering (
    drawInstructions,
    DrawInstruction(..),
    Renderable(..),
    Updateable(..)
) where

import Graphics.Gloss
import Engine.Core.Classes
import Engine.Core.Coordinate
import Engine.Graphics.Sprite

{- Data structures -}
data DrawInstruction = DrawInstruction Coordinate Sprite

{- Classes -}

{- Instances -}
instance Renderable DrawInstruction where
    render (DrawInstruction coordinate sprite) = let (Coordinate x y) = coordinate - halfScreenSize
                                                 in translate x (-y) (render sprite)

{- Functions -}
drawInstructions :: [DrawInstruction] -> Picture
drawInstructions instructions = pictures (map render instructions)





