module Engine.Graphics.Rendering (
    drawInstruction,
    Drawable(..)
) where

import Graphics.Gloss
import Engine.Coordinates
import Engine.Graphics.Models

drawInstruction :: [DrawInstruction] -> Picture
drawInstruction instructions = pictures (map draw instructions)

-- Drawable implementations for popular classes
class Drawable a where
    draw :: a -> Picture

instance Drawable Sprite where
    draw (StaticSprite frame) = frame
    draw (AnimatedSprite Animation{animState=AnimationState current _} frames) = frames!!current

instance Drawable DrawInstruction where
    draw (DrawInstruction coordinate sprite) = let (Coordinate x y) = coordinate - halfScreenSize
                                               in translate x (-y) (draw sprite)