module Resources where

import Engine.Graphics.Animation
import Engine.Graphics.Loading
import Constants


spritePacmanDie   = loadAnimatedSprite "pacman/die" 11 Single spriteInterval
spritePacmanStill = loadStaticSprite "pacman/up"
spritePacmanUp    = loadAnimatedSprite "pacman/up" 2 Repeating spriteInterval
spritePacmanDown  = loadAnimatedSprite "pacman/down" 2 Repeating spriteInterval
spritePacmanLeft  = loadAnimatedSprite "pacman/left" 2 Repeating spriteInterval
spritePacmanRight = loadAnimatedSprite "pacman/right" 2 Repeating spriteInterval

spriteBlinkyStill = loadStaticSprite "ghosts/blinky/up"
spriteBlinkyUp    = loadAnimatedSprite "ghosts/blinky/up" 1 Repeating spriteInterval
spriteBlinkyDown  = loadAnimatedSprite "ghosts/blinky/down" 1 Repeating spriteInterval
spriteBlinkyLeft  = loadAnimatedSprite "ghosts/blinky/left" 1 Repeating spriteInterval
spriteBlinkyRight = loadAnimatedSprite "ghosts/blinky/right" 1 Repeating spriteInterval

spriteClydeStill  = loadStaticSprite "ghosts/clyde/up"
spriteClydeUp     = loadAnimatedSprite "ghosts/clyde/up" 1 Repeating spriteInterval
spriteClydeDown   = loadAnimatedSprite "ghosts/clyde/down" 1 Repeating spriteInterval
spriteClydeLeft   = loadAnimatedSprite "ghosts/clyde/left" 1 Repeating spriteInterval
spriteClydeRight  = loadAnimatedSprite "ghosts/clyde/right" 1 Repeating spriteInterval

spriteInkyStill   = loadStaticSprite "ghosts/inky/up"
spriteInkyUp      = loadAnimatedSprite "ghosts/inky/up" 1 Repeating spriteInterval
spriteInkyDown    = loadAnimatedSprite "ghosts/inky/down" 1 Repeating spriteInterval
spriteInkyLeft    = loadAnimatedSprite "ghosts/inky/left" 1 Repeating spriteInterval
spriteInkyRight   = loadAnimatedSprite "ghosts/inky/right" 1 Repeating spriteInterval

spritePinkyStill  = loadStaticSprite "ghosts/pinky/up"
spritePinkyUp     = loadAnimatedSprite "ghosts/pinky/up" 1 Repeating spriteInterval
spritePinkyDown   = loadAnimatedSprite "ghosts/pinky/down" 1 Repeating spriteInterval
spritePinkyLeft   = loadAnimatedSprite "ghosts/pinky/left" 1 Repeating spriteInterval
spritePinkyRight  = loadAnimatedSprite "ghosts/pinky/right" 1 Repeating spriteInterval

spriteEyesUp      = loadStaticSprite "ghosts/eyes/up"
spriteEyesDown    = loadStaticSprite "ghosts/eyes/down"
spriteEyesLeft    = loadStaticSprite "ghosts/eyes/left"
spriteEyesRight   = loadStaticSprite "ghosts/eyes/right"

spriteScatterRight  = loadAnimatedSprite "ghosts/scatter" 1 Repeating spriteInterval