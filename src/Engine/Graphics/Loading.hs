module Engine.Graphics.Loading (
    loadStaticSprite,
    loadAnimatedSprite,
    loadStaticSpriteFile
) where

import Text.Printf
import Graphics.Gloss.Game
import Graphics.Gloss (Picture(..))
import Engine.Graphics.Sprite
import Engine.Graphics.Animation
import Constants

-- | Loads creates filenames for given resource by identifier and amount of sprites
spriteFileNames :: String -> Int -> [String]
spriteFileNames identifier n = (\i -> resourceDir ++ identifier ++ "/" ++ printf "%02d" i ++ ".png") <$> [0.. n]

-- | Loads an image from file. If failed, then throws errors
loadImage :: String -> Picture
loadImage file = case img of Bitmap{} -> Scale spriteScale spriteScale img
                             _        -> error ("Cannot load " ++ file)
                 where img = png file

-- | Load Sprites
loadStaticSpriteFromPath :: String -> Sprite
loadStaticSpriteFromPath path = createStaticSprite (loadImage path) path

loadStaticSprite :: String -> Sprite
loadStaticSprite identifier = loadStaticSpriteFromPath source
                              where source = head (spriteFileNames identifier 0)

loadStaticSpriteFile :: String -> Sprite
loadStaticSpriteFile identifier = loadStaticSpriteFromPath (resourceDir ++ identifier ++ ".png")

loadAnimatedSprite :: String -> Int -> AnimationType -> Float -> Sprite
loadAnimatedSprite identifier n animType interval = createAnimatedSprite animType frames interval
                                                    where frames = map loadStaticSpriteFromPath (spriteFileNames identifier n)

