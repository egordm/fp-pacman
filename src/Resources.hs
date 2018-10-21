module Resources () where

import Text.Printf
import Graphics.Gloss.Game
import Graphics.Gloss (Picture(..))
import Constants

-- | Loads creates filenames for given resource by identifier and amount of sprites
spriteFileNames :: String -> Int -> [String]
spriteFileNames identifier n = (\i -> resourceDir ++ identifier ++ "/" ++ (printf "%02d" i) ++ ".png") <$> [0.. n]

-- | Loads an image from file. If failed, then throws errors
loadImage :: String -> Picture
loadImage file = case img of Bitmap _ _ _ _ -> img
                             _              -> error ("Cannot load " ++ file)
                 where img = png file


-- | Loads sprites for all given images. Errors if none is found
loadSpritesImages :: [String] -> [Picture]
loadSpritesImages = map loadImage

-- | Load Sprites
