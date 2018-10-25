module Constants where

import Graphics.Gloss.Data.Color(black)

-- | Window name
gameName = "FP Pacman"
background = black

-- | Windows size constants
width, height, offset :: Int
width = 496
height = 560
offset = 100

-- | FPS ofcourse
fps :: Int
fps = 60

-- | Paths
resourceDir = "res/"

spriteScale :: Float
spriteScale = 2

-- | Sprite Interval
spriteInterval :: Float
spriteInterval = 0.08

-- | Game specifics
epsilon :: Float
epsilon = 0.5

pacmanSpeed :: Float
pacmanSpeed = 100

turnTolerance :: Float
turnTolerance = fromInteger tileSize * 0.2

tileSize :: Integer
tileSize = floor (8 * spriteScale)