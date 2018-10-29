module Constants where

import Graphics.Gloss.Data.Color(black)

-- | Window name
gameName = "FP Pacman"
background = black

-- | Windows size constants
width, height, offset :: Int
width = 448
height = 560
offset = 100

-- | FPS ofcourse
fps :: Int
fps = 60

-- | Paths
-- resourceDir = "../res/" --for interpreter
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
pacmanSpeed = 110

ghostSpeed, ghostSpeedScatter, ghostSpeedDead :: Float
ghostSpeed = 105
ghostSpeedScatter = 60
ghostSpeedDead = 180

scatterModeDuration, scatterModeEnding :: Float
scatterModeDuration = 7
scatterModeEnding = 3

turnTolerance :: Float
turnTolerance = fromInteger tileSize * 0.2

tileSize :: Integer
tileSize = floor (8 * spriteScale)

liveCount :: Int
liveCount = 3

scorePacdot, scorePowerpill, scoreEatGhost, scoreEatCherry :: Int
scorePacdot = 10
scorePowerpill = 20
scoreEatGhost = 100
scoreEatCherry = 200