module Engine.Graphics.Loading (
    loadStaticSprite,
    loadAnimatedSprite
) where

import Text.Printf
import Graphics.Gloss.Game
import Graphics.Gloss (Picture(..))
import Engine.Graphics.Sprite
import Engine.Graphics.Animation
import Constants

-- | Loads creates filenames for given resource by identifier and amount of sprites
spriteFileNames :: String -> Int -> [String]
spriteFileNames identifier n = (\i -> resourceDir ++ identifier ++ "/" ++ (printf "%02d" i) ++ ".png") <$> [0.. n]

-- | Loads an image from file. If failed, then throws errors
loadImage :: String -> Picture
loadImage file = case img of Bitmap _ _ _ _ -> Scale spriteScale spriteScale img
                             _              -> error ("Cannot load " ++ file)
                 where img = png file


-- | Loads sprites for all given images. Errors if none is found
loadImages :: [String] -> [Picture]
loadImages = map loadImage

-- | Load Sprites
loadStaticSprite :: String -> Sprite
loadStaticSprite identifier = createStaticSprite frame
                              where frame = head (loadImages (spriteFileNames identifier 1))

loadAnimatedSprite :: String -> Int -> AnimationType -> Float -> Sprite
loadAnimatedSprite identifier n animType interval = createAnimatedSprite animType frames interval
                                                    where frames = loadImages (spriteFileNames identifier n)
