module Constants where

import Graphics.Gloss.Data.Color(black)

-- | Window name
gameName = "FP Pacman"
background = black

-- | Windows size constants
width, height, offset :: Int
width = 372
height = 496
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